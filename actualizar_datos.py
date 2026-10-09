from xbbg import blp
import blpapi
import pandas as pd

tickers = [
    'mxipsagc index',
    'ibov index',
    'spxt index',
    'sx5t index',
    'clp curncy',
    'brl curncy',
    'eur curncy'
]

fecha_inicio = pd.Timestamp.today() - pd.DateOffset(years=10)

datos = blp.bdh(
    tickers=tickers,
    flds='PX_LAST',
    start_date=fecha_inicio
)

datos.to_pandas().to_excel('datos.xlsx', index=False)