#Tesina

Predictability study for temperature based of SubX models from October to April in South America

**Compute_anomalies.R**: Calculates anomalies (run in first) having daily values and climatology 

**Compute_ensmean.R**: Calculates ensamble mean for each model except NRL
**Compute_ensmean_NRL.R**: Calculates ensamble mean for NRL 

**ensamblemean_MODEL.R**: Rearrange data for further verification

**obs_to_RDS.R**: Download and arrange cpc analysis

**solucion.R**: Run after "ensamblemean_MODEL.R" to flip latitudes 

**compute_MMM.R**: Compute multi-model ensamble

**verify_MODEL.R**: Calculates scores and makes graphics between model and analysis data 
**corridas.R**: Can be use to run all verify scripts in one go
**verify.R**: Similar to Verify_MODEL.R, makes all calculations in one script. Easier to change.


**funciones.R**: Functions 

**poligonos.R**: Data analysis for restricted region 

**ext_mod.R**: Same as "verify_model.R" only in specific extreme weeks and regions. Run after poligonos.R
**MJO_obs.R**: Defines and searches for an active MJO event.
**MJO_mod.R**: Evaluates metrics for MJO active and inactive events. Run after MJO_obs.R

## Bonus Scripts ---------------------------------------------------------------

**MJO_verifMME.R**: Creates plot for metric differences only for MME, sorting by initial MJO phase
**Verify_lead.R**: Same as verify but for every lead day instead of weeks
**ClimatologiaCheck.R, check_IRI.R**: Both CPC analysis made to compare CPC with IRI. Plots monthly anomalies of CPC analysis obtained from the IRI website. This was done to check with same data from another source.
**check_miembros.R**: Plots monthly anomalies of CPC analysis obtained from the IRI website. This was done to check with same data from another source.

Borrar:, test2.R, borrar check_miembros, compute_mme
