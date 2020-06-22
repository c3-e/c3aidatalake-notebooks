import requests
import pandas as pd

def read_data_json(typename, api, body):
    """
    read_data_json directly accesses the C3.ai COVID-19 Data Lake APIs using the requests library, 
    and returns the response as a JSON, raising an error if the call fails for any reason.
    ------
    typename: The type you want to access, i.e. 'OutbreakLocation', 'LineListRecord', 'BiblioEntry', etc.
    api: The API you want to access, either 'fetch' or 'evalmetrics'.
    body: The spec you want to pass. For examples, see the API documentation.
    """
    response = requests.post(
        "https://api.c3.ai/covid/api/1/" + typename + "/" + api, 
        json = body, 
        headers = {
            'Accept' : 'application/json', 
            'Content-Type' : 'application/json'
        }
    )
    
    # if request failed, show exception
    if response.status_code != 200:
        raise Exception(response.json()["message"])
            
    return response.json()

def fetch(typename, body, get_all = False, remove_meta = True):
    """
    fetch accesses the C3.ai COVID-19 Data Lake using read_data_json, and converts the response into a Pandas dataframe. 
    fetch is used for all non-timeseries data in the C3.ai COVID-19 Data Lake, and will call read_data as many times 
    as required to access all of the relevant data for a given typename and body.
    ------
    typename: The type you want to access, i.e. 'OutbreakLocation', 'LineListRecord', 'BiblioEntry', etc.
    body: The spec you want to pass. For examples, see the API documentation.
    get_all: If True, get all records and ignore any limit argument passed in the body. If False, use the limit argument passed in the body. The default is False.
    remove_meta: If True, remove metadata about each record. If False, include it. The default is True.
    """
    if get_all:
        has_more = True
        offset = 0
        limit = 2000
        df = pd.DataFrame()

        while has_more:
            body['spec'].update(limit = limit, offset = offset)
            response_json = read_data_json(typename, 'fetch', body)
            new_df = pd.json_normalize(response_json['objs'])
            df = df.append(new_df)
            has_more = response_json['hasMore']
            offset += limit
            
    else:
        response_json = read_data_json(typename, 'fetch', body)
        df = pd.json_normalize(response_json['objs'])
        
    if remove_meta:
        df = df.drop(columns = [c for c in df.columns if ('meta' in c) | ('version' in c)])
    
    return df
    
def evalmetrics(typename, body, get_all = False, remove_meta = True):
    """
    evalmetrics accesses the C3.ai COVID-19 Data Lake using read_data_json, and converts the response into a Pandas dataframe.
    evalmetrics is used for all timeseries data in the C3.ai COVID-19 Data Lake.
    ------
    typename: The type you want to access, i.e. 'OutbreakLocation', 'LineListRecord', 'BiblioEntry', etc.
    body: The spec you want to pass. For examples, see the API documentation.
    get_all: If True, get all metrics and ignore limits on number of expressions and ids. If False, consider expressions and ids limits. The default is False.
    remove_meta: If True, remove metadata about each record. If False, include it. The default is True.
    """
    if get_all:
        expressions = body['spec']['expressions']
        ids = body['spec']['ids']
        df = pd.DataFrame()
        
        for ids_start in range(0, len(ids), 10):
            for expressions_start in range(0, len(expressions), 4):
                body['spec'].update(
                    ids = ids[ids_start : ids_start + 10],
                    expressions = expressions[expressions_start : expressions_start + 4]
                )
                response_json = read_data_json(typename, 'evalmetrics', body)
                new_df = pd.json_normalize(response_json['result'])
                new_df = new_df.apply(pd.Series.explode)
                df = pd.concat([df, new_df], axis = 1)
            
    else:
        response_json = read_data_json(typename, 'evalmetrics', body)
        df = pd.json_normalize(response_json['result'])
        df = df.apply(pd.Series.explode)

    # get the useful data out
    if remove_meta:
        df = df.filter(regex = 'dates|data|missing')
    
    # only keep one date column
    date_cols = [col for col in df.columns if 'dates' in col]
    keep_cols =  date_cols[:1] + [col for col in df.columns if 'dates' not in col]
    df = df.filter(items = keep_cols).rename(columns = {date_cols[0] : "dates"})
    df["dates"] = pd.to_datetime(df["dates"])
    
    return df

def getprojectionhistory(body, remove_meta = True):
    """
    getprojectionhistory accesses the C3.ai COVID-19 Data Lake using read_data_json, and converts the response into a Pandas dataframe.
    ------
    body: The spec you want to pass. For examples, see the API documentation.
    remove_meta: If True, remove metadata about each record. If False, include it. The default is True.
    """  
    response_json = read_data_json("outbreaklocation", 'getprojectionhistory', body)
    df = pd.json_normalize(response_json)
    df = df.apply(pd.Series.explode)

    # get the useful data out
    if remove_meta:
        df = df.filter(regex = 'dates|data|missing|expr')
    
    # only keep one date column
    date_cols = [col for col in df.columns if 'dates' in col]
    keep_cols =  date_cols[:1] + [col for col in df.columns if 'dates' not in col]
    df = df.filter(items = keep_cols).rename(columns = {date_cols[0] : "dates"})
    df["dates"] = pd.to_datetime(df["dates"])
    
    # rename columns to simplify naming convention
    df = df.rename(columns = lambda x: x.replace(".value", ""))
    
    return df