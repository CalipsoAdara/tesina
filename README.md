Tesina

Predictability study for temperature and precipitation based of SubX models from Octuber to March in Southamerica

Compute_anomalies.R: Calculates anomalies (run in first) having daily values and climatology 

Compute_ensmean.R: Calculates ensamble mean for each model except NRL
Compute_ensmean.R: Calculates ensamble mean for NRL 

ensamblemean_MODEL.R: Rearrange data for further verification

obs_to_RDS.R: Download and arrange cpc reanalysis

solucion.R: Run after "ensamblemean_MODEL.R" to flip latitudes 

compute_MMM.R: Compute multi-model ensamble

verify_MODEL.R: Calculates scores and makes graphics between model and reanalysis data 
corridas.R: Can be use to run all verify scripts in one go


ClimatologiaCheck.R, check_IRI.R: Both CPC analysis made to compare CPC with IRI.

funciones.R: Functions 

poligonos.R: Data analysis for restricted region 

ext_mod.R: Same as "verify_model.R" only in specific extreme weeks and regions. Run after poligonos.R