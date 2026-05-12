function dfs_out = rt_cc(dfs)
% dfs = rt_cc()
%   Create a data_fields object and setup all the buttons for realtime
%   plots
% dfs_out = rt_cc(dfs)
%   Use the data_fields object and setup all the buttons for realtime plots
if nargin < 1 || isempty(dfs)
  dfs = data_fields('title', 'citco2 Instrument', ...
    'Color', [.8 .8 1], ...
    'h_leading', 8, 'v_leading', 2, ...
    'btn_fontsize', 12, ...
    'txt_fontsize', 12);
  context_level = dfs.rt_init;
else
  context_level = 1;
end
dfs.start_col;
dfs.plot('tm', 'label', 'T Mbase', 'plots', {'tmsws','tmmfc','tmtd','tmcpu','tmram','tmd'});
dfs.plot('tmsws','label','SW Stat','vars',{'SWStat'});
dfs.plot('tmmfc','label','MF Ctr','vars',{'MFCtr'});
dfs.plot('tmtd','label','T Drift','vars',{'SysTDrift'});
dfs.plot('tmcpu','label','CPU','vars',{'CPU_Pct'});
dfs.plot('tmram','label','RAM','vars',{'memused'});
dfs.plot('tmd','label','Disk','vars',{'Disk'});
dfs.plot('a', 'label', 'Algo', 'plots', {'ap','ap3','ap4'});
dfs.plot('ap','label','P1','vars',{'AlgoP1'});
dfs.plot('ap3','label','P3','vars',{'AlgoP3'});
dfs.plot('ap4','label','P4','vars',{'AlgoP4'});
dfs.plot('wtx', 'label', 'WTX', 'plots', {'wtxrh','wtxt','wtxp','wtxsr'});
dfs.plot('wtxrh','label','RH','vars',{'RH_Open_Pct','RH_Close_Pct','WTX_RH'});
dfs.plot('wtxt','label','Temp','vars',{'WTX_AirT','WTX_DewPt'});
dfs.plot('wtxp','label','P','vars',{'WTX_AbsAirP'});
dfs.plot('wtxsr','label','Sol Rad','vars',{'WTX_SolRad'});
dfs.plot('ptb', 'label', 'PTB', 'plots', {'ptbp','ptbt','ptbstale'});
dfs.plot('ptbp','label','P','vars',{'PTB_P'});
dfs.plot('ptbt','label','T','vars',{'PTB_T'});
dfs.plot('ptbstale','label','stale','vars',{'PTB_dev_stale','PTB_drv_stale'});
dfs.end_col;
dfs.start_col;
dfs.plot('wtxw', 'label', 'WTX Wind', 'plots', {'wtxwd','wtxws','wtxwstatus','wtxwq'});
dfs.plot('wtxwd','label','Dir','vars',{'WTX_WindDir'});
dfs.plot('wtxws','label','Speed','vars',{'WTX_WindSpd','wslimit_fast','wslimit_slow'});
dfs.plot('wtxwstatus','label','Status','vars',{{'name','is_high','var_name','ws_high','bit_number',0},{'name','is_low','var_name','ws_low','bit_number',0}});
dfs.plot('wtxwq','label','Quality','vars',{'WTX_WindQ'});
dfs.plot('wtx_precip', 'label', 'WTX Precip', 'plots', {'wtx_precipt','wtx_precipa','wtx_precipi','wtx_precipdsc'});
dfs.plot('wtx_precipt','label','Type','vars',{'WTX_PrecipType'});
dfs.plot('wtx_precipa','label','Accum','vars',{'WTX_PrecipAcc'});
dfs.plot('wtx_precipi','label','Intensity','vars',{'WTX_PrecipInt'});
dfs.plot('wtx_precipdsc','label','DS 2C','vars',{{'name','STEnc_DS_2C','var_name','STEnc_status','bit_number',6}});
dfs.plot('wtxs', 'label', 'WTX Status', 'plots', {'wtxsf','wtxss'});
dfs.plot('wtxsf','label','Fresh','vars',{{'name','RH','var_name','WTX_Fresh','bit_number',0},{'name','WD','var_name','WTX_Fresh','bit_number',1},{'name','PTWQSR','var_name','WTX_Fresh','bit_number',2},{'name','AT','var_name','WTX_Fresh','bit_number',3},{'name','DP','var_name','WTX_Fresh','bit_number',4},{'name','WSPC','var_name','WTX_Fresh','bit_number',5},{'name','AP','var_name','WTX_Fresh','bit_number',6}});
dfs.plot('wtxss','label','Stale','vars',{'WTX_dev_stale','WTX_drv_stale'});
dfs.end_col;
dfs.start_col;
dfs.plot('st', 'label', 'ST', 'plots', {'stt','stm','sts','stt_int','sta','ste','str','stratio','ststatus','std','stv'});
dfs.plot('stt','label','Tdrift','vars',{'ST_Tdrift'});
dfs.plot('stm','label','Modus','vars',{'ST_modus'});
dfs.plot('sts','label','Stale','vars',{'ST_stale'});
dfs.plot('stt_int','label','t int','vars',{'ST_t_int'});
dfs.plot('sta','label','Azimuth','vars',{'ST_tpg_azi'});
dfs.plot('ste','label','Elevation','vars',{'ST_tpg_ele','Sol_ele'});
dfs.plot('str','label','Radiance','vars',{'model_intensity','WTX_SolRad'});
dfs.plot('stratio','label','Ratio','vars',{'radiance_ratio','Rad_Open_Pct','Rad_Close_Pct'});
dfs.plot('ststatus','label','Status','vars',{{'name','OK2Open','var_name','ok_to_open','bit_number',0},{'name','ST_flip_bit','var_name','ST_flip','bit_number',0}});
dfs.plot('std','label','Diff','vars',{'ST_azi_diff','ST_ele_diff'});
dfs.plot('stv','label','Volts','vars',{'ST_azi_volts','ST_ele_volts'});
dfs.plot('mks', 'label', 'MKS925', 'plots', {'mksp','mkst','mkss'});
dfs.plot('mksp','label','P','vars',{'Pump_P'});
dfs.plot('mkst','label','T','vars',{'MKS_T'});
dfs.plot('mkss','label','Stale','vars',{'MKS_stale'});
dfs.end_col;
dfs.start_col;
dfs.plot('ifs', 'label', 'IFS', 'plots', {'ifscs','ifsrn','ifsrs','ifssn','ifssr','ifssrw','ifsst','ifstr','ifsl'});
dfs.plot('ifscs','label','C Stat','vars',{'IFSCStat'});
dfs.plot('ifsrn','label','RN','vars',{'IFSRN'});
dfs.plot('ifsrs','label','R Stat','vars',{'IFSRStat'});
dfs.plot('ifssn','label','SN','vars',{'IFSSN'});
dfs.plot('ifssr','label','SR','vars',{'IFSSR'});
dfs.plot('ifssrw','label','Sl RW','vars',{'IFSSlR','IFSSlW'});
dfs.plot('ifsst','label','Src T','vars',{'IFSSrcT'});
dfs.plot('ifstr','label','TR','vars',{'IFSTR'});
dfs.plot('ifsl','label','Laser','vars',{'LasAAF','LasAOF','LasBAF','LasBOF'});
dfs.plot('ifshk', 'label', 'IFS HK', 'plots', {'ifshkt','ifshkp','ifshkifh','ifshks','ifshktd','ifshkstale'});
dfs.plot('ifshkt','label','Temps','vars',{'ScBlkT','IFSSrcT'});
dfs.plot('ifshkp','label','Press','vars',{'IFS_P'});
dfs.plot('ifshkifh','label','IF Hum','vars',{'IFHum'});
dfs.plot('ifshks','label','Status','vars',{{'name','DiagScanS','var_name','IFSDiag','bit_number',0},{'name','DiagDetS','var_name','IFSDiag','bit_number',1},{'name','DiagHeNeS','var_name','IFSDiag','bit_number',2},{'name','DiagIRSrcS','var_name','IFSDiag','bit_number',3},{'name','DiagAUS','var_name','IFSDiag','bit_number',4},{'name','DiagRdyS','var_name','IFSDiag','bit_number',5}});
dfs.plot('ifshktd','label','T Drift','vars',{'IFSDT'});
dfs.plot('ifshkstale','label','Stale','vars',{'IFSStale'});
dfs.end_col;
dfs.start_col;
dfs.plot('ln', 'label', 'LN2', 'plots', {'lnt','lnp','lnd','lns','lnps','lnas','lnstale'});
dfs.plot('lnt','label','Temp','vars',{'LN2TankT','InSbT'});
dfs.plot('lnp','label','P','vars',{'LN2P'});
dfs.plot('lnd','label','Depth','vars',{'LN2Depth'});
dfs.plot('lns','label','Status','vars',{{'name','LN2Pump','var_name','LN2Stat','bit_number',1},{'name','LN2Valve','var_name','LN2Stat','bit_number',2},{'name','LN2TMB','var_name','LN2Stat','bit_number',3},{'name','LN2Init','var_name','LN2DrvStat','bit_number',0}});
dfs.plot('lnps','label','P Stat','vars',{'LN2PStat'});
dfs.plot('lnas','label','A Stat','vars',{{'name','LN2Alarm','var_name','LN2Stat','bit_number',7},{'name','LN2_VES_AL','var_name','LN2AlarmStat','bit_number',1},{'name','LN2_TMB_AL','var_name','LN2AlarmStat','bit_number',2},{'name','LN2_EXS_AL','var_name','LN2AlarmStat','bit_number',3},{'name','LN2_MNS_AL','var_name','LN2AlarmStat','bit_number',4},{'name','LN2_BLK_AL','var_name','LN2AlarmStat','bit_number',5},{'name','LN2_HOT_AL','var_name','LN2AlarmStat','bit_number',8},{'name','LN2_FRZ_AL','var_name','LN2AlarmStat','bit_number',9},{'name','LN2_NOP_AL','var_name','LN2AlarmStat','bit_number',10}});
dfs.plot('lnstale','label','Stale','vars',{'LN2Stale'});
dfs.plot('p', 'label', 'Power', 'plots', {'ps','pstale'});
dfs.plot('ps','label','Status','vars',{{'name','WP_Pump_S','var_name','webpower_status','bit_number',0},{'name','WP_PDISO8_S','var_name','webpower_status','bit_number',1},{'name','WP_MKS925_S','var_name','webpower_status','bit_number',2},{'name','WP_AXIS_P1378_S','var_name','webpower_status','bit_number',3}});
dfs.plot('pstale','label','Stale','vars',{'webpower_stale'});
dfs.plot('e', 'label', 'Enclosure', 'plots', {'es','estale'});
dfs.plot('es','label','Status','vars',{{'name','STEnc_Close_Limit','var_name','STEnc_status','bit_number',0},{'name','STEnc_Open_Limit','var_name','STEnc_status','bit_number',1},{'name','STEnc_Operating','var_name','STEnc_status','bit_number',2},{'name','STEnc_Weather','var_name','STEnc_status','bit_number',3},{'name','STEnc_Power','var_name','STEnc_status','bit_number',4},{'name','STEnc_DS_2C','var_name','STEnc_status','bit_number',6},{'name','STEnc_Error','var_name','STEnc_status','bit_number',7},{'name','STEnc_Close_Relay','var_name','STEnc_status','bit_number',8},{'name','STEnc_Open_Relay','var_name','STEnc_status','bit_number',9},{'name','STEnc_ASE_DS_2C_MAN','var_name','STEnc_status','bit_number',13},{'name','STEnc_ASE_DS_2C_STBY','var_name','STEnc_status','bit_number',12}});
dfs.plot('estale','label','Stale','vars',{'STEnc_stale'});
dfs.end_col;
dfs.set_connection('127.0.0.1', 1506);
if nargout > 0
  dfs_out = dfs;
else
  dfs.resize(context_level);
end
