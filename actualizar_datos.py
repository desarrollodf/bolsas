"""Descarga de datos Bloomberg y generación de gráficos del IPSA."""

import re
from datetime import date
from pathlib import Path

import matplotlib.dates as mdates
import matplotlib.patheffects as pe
import matplotlib.pyplot as plt
import pandas as pd
import requests
from dateutil.relativedelta import relativedelta
from matplotlib import font_manager
from PIL import Image, ImageFilter


# ============================================================
# Configuración general
# ============================================================

TONO = "#061763"

# Conversión aproximada de unidades ggplot -> matplotlib
MM_A_PT = 72.27 / 25.4
LW_LINEA = 1.5 * MM_A_PT * 72 / 96


# Carpeta donde está ubicado este script
try:
    CARPETA = Path(__file__).resolve().parent
except NameError:
    # Para ejecución desde notebook / consola interactiva
    CARPETA = Path.cwd()


CARPETA_FUENTES = CARPETA / "fuentes"

RUTA_DATOS = CARPETA / "datos.xlsx"
RUTA_FOTO = CARPETA / "bolsa_stgo.jpg"

RUTA_GRAFICO = CARPETA / "grafico_ipsa.png"
RUTA_TRANSPARENTE = CARPETA / "ipsa_transparente.png"


TICKERS = [
    "mxipsagc index",
    "ibov index",
    "spxt index",
    "sx5t index",
    "clp curncy",
    "brl curncy",
    "eur curncy",
]


# ============================================================
# Datos Bloomberg
# ============================================================

def descargar_datos(
    tickers=TICKERS,
    anios=10,
    ruta_excel=RUTA_DATOS,
):
    """
    Descarga precios históricos desde Bloomberg mediante xbbg.

    Devuelve un DataFrame largo con columnas:
    ticker, date, value.
    """

    from xbbg import blp

    fecha_inicio = pd.Timestamp.today() - pd.DateOffset(years=anios)

    print(
        f"Descargando datos Bloomberg desde "
        f"{fecha_inicio.strftime('%Y-%m-%d')}...",
        flush=True,
    )

    datos = blp.bdh(
        tickers=tickers,
        flds="PX_LAST",
        start_date=fecha_inicio,
    )

    bolsas_bruto = normalizar_bdh(datos)

    bolsas_bruto.to_excel(
        ruta_excel,
        index=False,
    )

    print(
        f"datos.xlsx actualizado correctamente: {ruta_excel}",
        flush=True,
    )

    print(
        f"Última fecha descargada: "
        f"{bolsas_bruto['date'].max().strftime('%Y-%m-%d')}",
        flush=True,
    )

    print(
        f"Filas generadas: {len(bolsas_bruto):,}",
        flush=True,
    )

    return bolsas_bruto


def normalizar_bdh(datos):
    """
    Convierte la salida de Bloomberg bdh a formato largo:

    ticker | date | value

    El ticker queda normalizado en minúsculas.
    """

    df = (
        datos.to_pandas()
        if hasattr(datos, "to_pandas")
        else datos.copy()
    )

    # Formato ancho típico de xbbg:
    # MultiIndex de columnas (ticker, field)
    if isinstance(df.columns, pd.MultiIndex):

        df = (
            df
            .droplevel(-1, axis=1)
            .rename_axis("date")
            .reset_index()
            .melt(
                id_vars="date",
                var_name="ticker",
                value_name="value",
            )
        )

    # Formato largo
    elif {"ticker", "date", "value"}.issubset(df.columns):

        if "field" in df.columns:
            df = df[
                df["field"].astype(str).str.upper() == "PX_LAST"
            ]

        df = df[
            [
                "ticker",
                "date",
                "value",
            ]
        ]

    else:
        raise ValueError(
            "Formato de bdh no reconocido. "
            f"Columnas recibidas: {list(df.columns)}"
        )

    df["ticker"] = (
        df["ticker"]
        .astype(str)
        .str.lower()
        .str.strip()
    )

    df["date"] = pd.to_datetime(
        df["date"],
        errors="coerce",
    )

    df["value"] = pd.to_numeric(
        df["value"],
        errors="coerce",
    )

    return (
        df
        .dropna(subset=["ticker", "date"])
        .sort_values(["ticker", "date"])
        .reset_index(drop=True)
    )


# ============================================================
# Fuente Google Fonts
# ============================================================

def font_add_google(
    nombre="Antonio",
    pesos=(400, 700),
):
    """
    Descarga una fuente desde Google Fonts si no existe
    localmente y la registra en matplotlib.

    Los archivos se guardan en ./fuentes/.
    """

    CARPETA_FUENTES.mkdir(
        parents=True,
        exist_ok=True,
    )

    css = None

    for peso in pesos:

        ruta = (
            CARPETA_FUENTES
            / f"{nombre}-{peso}.ttf"
        )

        if not ruta.exists():

            if css is None:

                respuesta_css = requests.get(
                    "https://fonts.googleapis.com/css",
                    params={
                        "family":
                            f"{nombre}:{','.join(map(str, pesos))}"
                    },
                    headers={
                        "User-Agent":
                            "Mozilla/5.0 (X11; U; Linux)"
                    },
                    timeout=30,
                )

                respuesta_css.raise_for_status()
                css = respuesta_css.text

            bloques = re.findall(
                r"@font-face\s*{[^}]*}",
                css,
            )

            bloque = next(
                (
                    b
                    for b in bloques
                    if f"font-weight: {peso};" in b
                ),
                None,
            )

            if bloque is None:
                raise RuntimeError(
                    f"No se encontró peso {peso} "
                    f"para la fuente {nombre}."
                )

            match_url = re.search(
                r"url\((.*?)\)",
                bloque,
            )

            if match_url is None:
                raise RuntimeError(
                    f"No se encontró URL para "
                    f"{nombre} {peso}."
                )

            url = match_url.group(1)

            respuesta_fuente = requests.get(
                url,
                timeout=30,
            )

            respuesta_fuente.raise_for_status()

            ruta.write_bytes(
                respuesta_fuente.content
            )

        font_manager.fontManager.addfont(
            str(ruta)
        )

    return nombre


# ============================================================
# Fondo
# ============================================================

def editar_fondo(
    ruta,
    color,
    opacity=80,
    sigma=20,
):
    """
    Aplica colorización y desenfoque al fondo.

    Equivalente aproximado a:
    magick::image_colorize()
    + image_blur().
    """

    img = Image.open(ruta).convert("RGB")

    capa = Image.new(
        "RGB",
        img.size,
        color,
    )

    img = Image.blend(
        img,
        capa,
        opacity / 100,
    )

    return img.filter(
        ImageFilter.GaussianBlur(sigma)
    )


def bg_cover(
    ruta,
    px_w,
    px_h,
    color="#D9FEEF",
    opacity=80,
    sigma=20,
):
    """
    Ajusta una imagen como CSS background-size: cover.

    Mantiene proporción y recorta el exceso,
    evitando deformaciones.
    """

    img = editar_fondo(
        ruta,
        color,
        opacity,
        sigma,
    )

    escala = max(
        px_w / img.width,
        px_h / img.height,
    )

    img = img.resize(
        (
            round(img.width * escala),
            round(img.height * escala),
        ),
        Image.LANCZOS,
    )

    izq = max(
        0,
        (img.width - px_w) // 2,
    )

    sup = max(
        0,
        (img.height - px_h) // 2,
    )

    return img.crop(
        (
            izq,
            sup,
            izq + px_w,
            sup + px_h,
        )
    )


# ============================================================
# Utilidades
# ============================================================

def formato_cl(
    valor,
    decimales=2,
):
    """
    Formato numérico estilo chileno:

    12.345,67
    """

    return (
        f"{valor:,.{decimales}f}"
        .replace(",", "X")
        .replace(".", ",")
        .replace("X", ".")
    )


def _eje_vacio(
    fig,
    transparente=False,
):
    """
    Crea un eje sin elementos visuales.
    """

    ax = fig.add_axes(
        [0, 0, 1, 1]
    )

    ax.set_axis_off()

    if transparente:
        ax.patch.set_alpha(0)

    return ax


def _limites(
    ax,
    x_ini,
    x_fin,
    y_min,
    y_max,
    mult=0.05,
):
    """
    Expansión aproximada de 5 %, similar a ggplot.
    """

    dx = (x_fin - x_ini) * mult
    dy = (y_max - y_min) * mult

    # Evitar rango Y igual a cero
    if dy == 0:
        dy = abs(y_max) * mult or 1

    ax.set_xlim(
        x_ini - dx,
        x_fin + dx,
    )

    ax.set_ylim(
        y_min - dy,
        y_max + dy,
    )


# ============================================================
# Preparación IPSA
# ============================================================

def preparar_ipsa(
    bolsas_bruto,
    ticker="mxipsagc index",
):
    """
    Filtra el IPSA y conserva aproximadamente
    los últimos 12 meses.
    """

    ipsa_pre = bolsas_bruto[
        bolsas_bruto["ticker"].str.lower() == ticker.lower()
    ].copy()

    ipsa_pre["date"] = pd.to_datetime(
        ipsa_pre["date"],
        errors="coerce",
    )

    ipsa_pre["value"] = pd.to_numeric(
        ipsa_pre["value"],
        errors="coerce",
    )

    validos = (
        ipsa_pre
        .dropna(subset=["date", "value"])
        .sort_values("date")
    )

    if validos.empty:
        raise ValueError(
            "No se encontraron datos válidos del IPSA."
        )

    # Último dato del año anterior
    previo = validos[
        validos["date"].dt.year
        == date.today().year - 1
    ]

    fecha_lastyr = (
        previo["date"].iloc[-1]
        if not previo.empty
        else None
    )

    fecha_max = validos["date"].max()

    ipsa = validos[
        validos["date"]
        >= fecha_max - relativedelta(years=1)
    ].copy()

    ipsa = (
        ipsa
        .sort_values("date")
        .reset_index(drop=True)
    )

    return ipsa, fecha_lastyr


# ============================================================
# Gráfico con foto de fondo
# ============================================================

def grafico_con_foto(
    ipsa,
    ruta_imagen,
    ruta_salida=RUTA_GRAFICO,
    fuente="Antonio",
):

    if not Path(ruta_imagen).exists():
        raise FileNotFoundError(
            f"No se encontró la imagen de fondo: "
            f"{ruta_imagen}"
        )

    ultimo_value = ipsa["value"].iloc[-1]
    ultima_date = ipsa["date"].iloc[-1]

    ultima_date_format = (
        ultima_date.strftime("%d-%m-%Y")
    )

    fechas = mdates.date2num(
        ipsa["date"]
    )

    valores = (
        ipsa["value"]
        .astype(float)
        .to_numpy()
    )

    x_inicio = fechas.min()
    x_final = fechas.max()
    x_total = x_final - x_inicio

    y_min = valores.min()
    y_max = valores.max()
    y_total = y_max - y_min

    if x_total == 0:
        x_total = 1

    if y_total == 0:
        y_total = abs(y_max) * 0.05 or 1

    fig = plt.figure(
        figsize=(6, 4),
        dpi=300,
    )

    # ========================================================
    # Fondo sin deformación
    # ========================================================

    px_w = round(
        fig.get_figwidth()
        * fig.dpi
    )

    px_h = round(
        fig.get_figheight()
        * fig.dpi
    )

    fondo = bg_cover(
        ruta_imagen,
        px_w,
        px_h,
        color="#D8DCF4",
        opacity=80,
        sigma=20,
    )

    ax_bg = _eje_vacio(fig)

    ax_bg.imshow(
        fondo,
        aspect="auto",
    )

    # ========================================================
    # Gráfico
    # ========================================================

    ax = _eje_vacio(
        fig,
        transparente=True,
    )

    def texto(
        x_frac,
        y_frac,
        label,
        color,
        size,
        **kw,
    ):

        ax.text(
            x_inicio + x_total * x_frac,
            y_max + y_total * y_frac,
            "\n" + label,
            color=color,
            fontsize=size * MM_A_PT,
            fontweight="bold",
            family=fuente,
            ha="left",
            va="center",
            zorder=3,
            **kw,
        )

    texto(
        0.02,
        0.30,
        "MSCI IPSA",
        TONO,
        12,
    )

    texto(
        0.407,
        0.30,
        "12",
        TONO,
        12,
    )

    texto(
        0.352,
        0.25,
        "EN",
        "#191919",
        7,
    )

    texto(
        0.489,
        0.25,
        "MESES",
        "#191919",
        7,
    )

    texto(
        0.65,
        0.30,
        f"{formato_cl(ultimo_value)} pts.",
        "white",
        9,
        path_effects=[
            pe.withStroke(
                linewidth=2,
                foreground="black",
            )
        ],
    )

    texto(
        0.75,
        0.13,
        ultima_date_format,
        "#191919",
        4,
    )

    ax.plot(
        fechas,
        valores,
        color=TONO,
        linewidth=LW_LINEA,
        zorder=4,
    )

    ax.plot(
        fechas[-1],
        valores[-1],
        "o",
        color=TONO,
        markersize=3 * MM_A_PT,
        zorder=5,
    )

    # El eje Y también debe abarcar los textos
    _limites(
        ax,
        x_inicio,
        x_final,
        y_min,
        y_max + y_total * 0.30,
    )

    fig.savefig(
        ruta_salida,
        dpi=300,
        bbox_inches=None,
        pad_inches=0,
    )

    plt.close(fig)

    print(
        f"Gráfico generado: {ruta_salida}",
        flush=True,
    )

    return ruta_salida


# ============================================================
# Gráfico transparente
# ============================================================

def grafico_transparente(
    ipsa,
    ruta_salida=RUTA_TRANSPARENTE,
    width_in=10.8,
    height_in=14.4,
    dpi=300,
):

    fechas = mdates.date2num(
        ipsa["date"]
    )

    valores = (
        ipsa["value"]
        .astype(float)
        .to_numpy()
    )

    fig = plt.figure(
        figsize=(width_in, height_in),
        dpi=dpi,
    )

    fig.patch.set_alpha(0)

    ax = _eje_vacio(
        fig,
        transparente=True,
    )

    ax.plot(
        fechas,
        valores,
        color=TONO,
        linewidth=LW_LINEA,
        zorder=4,
    )

    ax.plot(
        fechas[-1],
        valores[-1],
        "o",
        color=TONO,
        markersize=3 * MM_A_PT,
        zorder=5,
    )

    _limites(
        ax,
        fechas.min(),
        fechas.max(),
        valores.min(),
        valores.max(),
    )

    fig.savefig(
        ruta_salida,
        dpi=dpi,
        transparent=True,
        bbox_inches=None,
        pad_inches=0,
    )

    plt.close(fig)

    print(
        f"Gráfico transparente generado: "
        f"{ruta_salida}",
        flush=True,
    )

    return ruta_salida


# ============================================================
# Generación completa
# ============================================================

def generar_graficos_ipsa(
    bolsas_bruto,
    ruta_imagen=RUTA_FOTO,
):

    ipsa, fecha_lastyr = preparar_ipsa(
        bolsas_bruto
    )

    print(
        f"Último dato IPSA: "
        f"{ipsa['date'].iloc[-1].strftime('%Y-%m-%d')} "
        f"= {ipsa['value'].iloc[-1]:,.2f}",
        flush=True,
    )

    if fecha_lastyr is not None:
        print(
            f"Último dato del año anterior: "
            f"{fecha_lastyr.strftime('%Y-%m-%d')}",
            flush=True,
        )

    print(
        "Preparando fuente Antonio...",
        flush=True,
    )

    fuente = font_add_google(
        "Antonio"
    )

    grafico_con_foto(
        ipsa,
        ruta_imagen,
        RUTA_GRAFICO,
        fuente,
    )

    grafico_transparente(
        ipsa,
        RUTA_TRANSPARENTE,
    )

    return ipsa


# ============================================================
# Ejecución
# ============================================================

def main():

    print(
        "========================================",
        flush=True,
    )

    print(
        "Actualización Bloomberg + gráficos IPSA",
        flush=True,
    )

    print(
        "========================================",
        flush=True,
    )

    bolsas_bruto = descargar_datos()

    generar_graficos_ipsa(
        bolsas_bruto,
        ruta_imagen=RUTA_FOTO,
    )

    print(
        "Proceso completado correctamente.",
        flush=True,
    )

    print(
        f"Archivos generados:\n"
        f"- {RUTA_DATOS.name}\n"
        f"- {RUTA_GRAFICO.name}\n"
        f"- {RUTA_TRANSPARENTE.name}",
        flush=True,
    )


if __name__ == "__main__":
    main()
