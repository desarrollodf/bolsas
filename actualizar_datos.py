from xbbg import blp
import blpapi
import pandas as pd

tickers = [
    'co1 comdty',
    'cl1 comdty',
    'xb1 comdty',
    'ho1 comdty',
    'ng1 comdty',
    'tzt1 comdty'
]

fecha_inicio = pd.Timestamp.today() - pd.DateOffset(years=10)

datos = blp.bdh(
    tickers=tickers,
    flds='PX_LAST',
    currency='USD',
    start_date=fecha_inicio
)

datos.to_pandas().to_excel('datos.xlsx', index=False)