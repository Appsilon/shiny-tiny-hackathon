# config.R - Configuración general de la aplicación
# Título de la aplicación
APP_TITLE <- "FDA Adverse Events Reporting System (FAERS) Public Dashboard"

# Rutas de archivos
FILE_PATHS <- list(
  all_reporters = "data/report_type.parquet",
  consumer = "data/reporter.parquet",
  healthcare = "data/reporter.parquet",
  reporter_region = "data/reporter_region.parquet",
  seriousness = "data/report_serioursness.parquet",
  age_group = "data/age_group.parquet",
  sex = "data/sex.parquet"
)

# Opciones para el filtro de reportero (usando los nombres de archivos directamente)
REPORTER_OPTIONS <- c(
  "report_type", 
  "reporter", 
  "reporter_region",
  "report_serioursness",
  "age_group",
  "sex"
)

# Colores para los gráficos
CHART_COLORS <- list(
  # Colores generales
  primary = '#4682B4',  # Azul Acero
  secondary = '#90EE90', # Verde Claro
  tertiary = '#D3D3D3',  # Gris Claro
  quaternary = '#20B2AA', # Verde Azulado
  
  # Colores específicos
  consumer = '#4682B4',
  healthcare = '#90EE90',
  not_specified = '#D3D3D3',
  other = '#20B2AA',
  
  # Colores para seriedad
  serious = '#FFB347',  # Naranja
  death = '#FF6B6B',    # Rojo
  non_serious = '#90EE90', # Verde
  
  # Colores para sexo
  female = '#FF69B4',  # Rosa
  male = '#4682B4',    # Azul
  unknown = '#D3D3D3'  # Gris
)

# Variables de texto para pie de página
FOOTER_INTRO_TEXT <- "This page displays the number of adverse event reports received by FDA for drugs and therapeutic biologic products by the following Report Types."

FOOTER_DIRECT_REPORTS_TEXT <- "Direct Reports are voluntarily submitted directly to FDA through the MedWatch program by consumers and healthcare professionals."

FOOTER_MANDATORY_REPORTS_TEXT <- "Mandatory Reports are submitted by manufacturers and are categorized as:"

FOOTER_EXPEDITED_REPORTS_TEXT <- "Expedited reports that contain at least one adverse event that is not currently described in the product labeling and for which the patient outcome is serious, or"

FOOTER_NONEXPEDITED_REPORTS_TEXT <- "Non-expedited reports that do not meet the criteria for expedited reports, including cases that are reported as Serious and expected, Non-serious and unexpected and Non-serious and expected."

FOOTER_BSR_REPORTS_TEXT <- "BSR Reports are 15-day Biologic Safety Reports which were submitted to FDA as a separate report type until 2005."

# Texto de información adicional del pie de página
FOOTER_ADDITIONAL_TEXT <- ""