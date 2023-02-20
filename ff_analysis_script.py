# -*- coding: utf-8 -*-
"""
Created on Mon Dec 19 11:35:38 2022

@author: Lucja Doradzinska <l.doradzinska@nencki.edu.pl>
"""

from collections import OrderedDict 
import os

import erp_exp_ld as my

#path to ff_lab_mn module
os.chdir('E:\\ff_prep_scripts\\') 
import ff_lab_mn

# path to experiments folder
path='D:\\ff_experiment\\'

# data structure
logfile_folder = 'Logfiles\\'
beh_folder = 'Results\\behavior\\'
raw_beh_df_folder = beh_folder + 'raw_beh_df\\'
beh_log_folder = beh_folder + 'beh_logs\\'
beh_plot_folder = beh_folder + 'beh_plots\\'

raw_signal_folder = 'Raw_EEG\\'
eeg_folder = 'Results\\EEG\\'
eeg_log_folder = eeg_folder
raw_prep_folder = eeg_folder + 'raws_prep\\'
filt_folder = eeg_folder + 'filt\\'
epochs_raw_folder = eeg_folder + 'epochs_raw\\'
epochs_noeog_folder = eeg_folder + 'epochs_noeog\\'
epochs_clean_folder = eeg_folder + 'epochs_clean\\'
evokeds_folder = eeg_folder + 'evokeds\\'
erps_folder = eeg_folder + 'ERPs\\'
comps_long_folder = eeg_folder + 'comps_long\\'
#%% VARIABLES

subjects = ['009', '010', '011', '012', '013', '014', '015', '016', '017', '018', 
            '019', '020', '021', '022', '023', '024', '025', '026', '027', '028', 
            '029', '030', '031', '032', '033', '034', '035', '036', '037', '038',
            '039', '040', '041', '042', '043', '044', '045', '046', '047', '048',
            '049']

#BEHAVIORAL CODING
log_extention = '_dp_emo_blocks.log'
beh_stim_coding = OrderedDict({'task':{'DP':'dp', 'ID':'id'},
                               'masking':{'masked':'mask', 'unmasked':'unmask'},
                               'left_face':{'F': 'E', 'N':'N'}, 
                               'right_face':{'F': 'E', 'N':'N'}, 
                               'gender':{'fem':'fem', 'male':'male'},
                               'left_mouth':{'open':'O', 'closed':'C'},
                               'right_mouth':{'open':'O', 'closed':'C'},
                               'dot_side':{'left':'l', 'right':'r'},
                               'dot_orient':{'vert': 'v', 'horiz':'h'}})
valid_resps = {'201':'incorrect', '211':'correct', '202':'incorrect', '212':'correct'}
quest_codes = ['dot']
trial_end_codes = ['fix', 'break', 'end', 'inst']

#EEG CHANNELS
stim_channel = 'Status'
montage_kind = 'biosemi64'
veog_channels = ['EXG1', 'EXG2']
heog_channels = ['EXG3', 'EXG4']
exclude_channels = ['EXG7', 'EXG8']
reref_channels = ['EXG5', 'EXG6']

#EEG MARKERS
eeg_stim_coding = OrderedDict({'task':{'DP':1, 'ID':65},
                               'masking':{'masked':0, 'unmasked':32},
                               'faces':{'F-F':0, 'F-N':8, 'N-F':16, 'N-N':24},
                               'gender':{'fem':0, 'male':4}})
reduce_factors = ['gender']
markers = ff_lab_mn.Eeg_markers_manager(eeg_stim_coding, reduce_factors)
quest_markers =[150, 151, 152, 153]                           
resp_markers = [201, 211, 202, 212]

#ERP CONDITIONS
symm_contrast = {'task':['DP', 'ID'], 'masking':['masked', 'unmasked'], 'faces':['F-F', 'N-N']}
lat_contrast = {'task':['DP', 'ID'], 'masking':['masked', 'unmasked'], 'faces':['F-N', 'N-F']}

#%% LOGFILES TO DATAFRAME
new_conditions = OrderedDict({'contrast':{'congr':[{'left_face':'F', 'right_face':'N', 'dot_side':'left'}, 
                                                   {'left_face':'N', 'right_face':'F', 'dot_side':'right'}],
                                          'incongr':[{'left_face':'F', 'right_face':'N', 'dot_side':'right'}, 
                                                     {'left_face':'N', 'right_face':'F', 'dot_side':'left'}],
                                            
                                          'fear-symm':[{'left_face':'F', 'right_face':'F'}],
                                          'neutr-symm':[{'left_face':'N', 'right_face':'N'}]},
                              
                              'emo_congr':{'congr':[{'left_face':'F', 'right_face':'N'}, 
                                                    {'left_face':'N', 'right_face':'F'}],
                                           'incongr':[{'left_face':'F', 'right_face':'F'}, 
                                                      {'left_face':'N', 'right_face':'N'}]}, 
                              
                              'target':{'fearful':[{'left_face':'F', 'dot_side':'left'}, 
                                                   {'right_face':'F', 'dot_side':'right'}],
                                         'neutral':[{'right_face':'N', 'dot_side':'right'}, 
                                                    {'left_face':'N', 'dot_side':'left'}]},
                              
                              'dist_face':{'fearful':[{'left_face':'F', 'dot_side':'right'}, 
                                                      {'right_face':'F', 'dot_side':'left'}],
                                           'neutral':[{'right_face':'N', 'dot_side':'left'}, 
                                                      {'left_face':'N', 'dot_side':'right'}]}})


factors = ff_lab_mn.get_beh_factors(beh_stim_coding, new_conditions)
stim_conditions = ff_lab_mn.get_stim_conditions(beh_stim_coding, new_conditions)

my.beh.logfiles_to_array(subjects, path, logfile_folder, log_extention, raw_beh_df_folder, factors, stim_conditions, quest_codes, trial_end_codes, valid_resps)

#%% BEHAVIORAL ANALYSIS

beh_analizer = my.beh.Beh_analizer(subjects, path, raw_beh_df_folder, beh_log_folder)

#CONTROL
control_combine = {'task':['DP', 'ID'],
                   'masking':['masked', 'unmasked']}
control_compare = ff_lab_mn.beh_combine_conds(control_combine)
control_resps_count_log = beh_analizer.summarize_corr_resps('control_resp_log', control_compare, pool_all = True)
control_accuracy_log = beh_analizer.calculate_accuracy('control_accuracy_log', control_compare, pool_all = True)
 
#MAIN ANALYSIS
main_combine = {'task':['DP', 'ID'],
                'masking':['masked', 'unmasked'],
                'contrast':['congr', 'incongr', 'fear-symm', 'neutr-symm']}
main_compare = ff_lab_mn.beh_combine_conds(main_combine)
resp_count_log = beh_analizer.summarize_corr_resps('resp_log', main_compare, save_csv = True)
accuracy_log = beh_analizer.calculate_accuracy('accuracy_log', main_compare, save_csv = True)
agg_RT_df = beh_analizer.aggregate_RTs ('median_RT', main_compare, 200, 1600, save_csv = True)

#ACCURACY BY EMOTION
emo_combine = {'task':['ID'],
               'masking':['masked', 'unmasked'],
               'target':['fearful', 'neutral']}
emo_compare = ff_lab_mn.beh_combine_conds(emo_combine)
accuracy_log = beh_analizer.calculate_accuracy('emo_acc_log', emo_compare, save_csv = True)

#STD ANALYSIS
d_combine = {'task':['ID'],
             'masking':['masked', 'unmasked']}
d_compare = ff_lab_mn.beh_combine_conds(d_combine)
signal = {'target':['fearful']}
noise = {'target':['neutral']}
d_log, c_log, hit_fa_log = beh_analizer.calculate_sdt_params('ID_mask_vs_unmask', d_compare, signal, noise, d_corr = True, 
                                                             save_hit_fa_log = True, save_csv = True)


d_combine = {'task':['ID'],
             'masking':['masked', 'unmasked'],
             'emo_congr':['congr', 'incongr'] }
d_compare = ff_lab_mn.beh_combine_conds(d_combine)
signal = {'target':['fearful'], }
noise = {'target':['neutral']}
d_log, c_log, hit_fa_log = beh_analizer.calculate_sdt_params('ID_emo_congr', d_compare, signal, noise, d_corr = True, save_csv = True)

d_combine = {'task':['ID'],
             'masking':['masked', 'unmasked'],
             'dist_face':['fearful', 'neutral'] }
d_compare = ff_lab_mn.beh_combine_conds(d_combine)
signal = {'target':['fearful'], }
noise = {'target':['neutral']}
d_log, c_log, hit_fa_log = beh_analizer.calculate_sdt_params('ID_dist_face', d_compare, signal, noise, d_corr = True, save_csv = True)
#%% EEG PREPROCESSING

#REREFERENCING RAW DATA
my.eeg_prep.raw_prep(subjects, path, raw_signal_folder, raw_prep_folder, stim_channel, set_eog = True, 
                     veog_channels = veog_channels, heog_channels = heog_channels, exclude_channels = exclude_channels, 
                     reref_channels = reref_channels)

#FILTERING RAW DATA
my.eeg_prep.filter_raw(subjects, path, raw_prep_folder, filt_folder, plot_psd = True, save_psd_plots = True)

# EPOCHING, REJECTING INVALID TRIALS AND RESAMPLING
stim_markers = markers.get_markers()

invalid_resp_log = my.eeg_prep.to_epochs(subjects, path, filt_folder, epochs_raw_folder, eeg_log_folder, stim_channel, 
                                         stim_markers, resp_markers, quest_markers,resample = True)

# REMOVING EOGS FROM EPOCHS
eog_artifacts_log = my.eeg_prep.remove_eogs(subjects, path, epochs_raw_folder, epochs_noeog_folder, eeg_log_folder, 
                                            reject_veog = True, reject_heog = True, perform_ica = True)

# CLEANING EPOCHS FROM ARTIFACTS
epochs_clean_log = my.eeg_prep.clean_epochs(subjects, path, epochs_noeog_folder, epochs_clean_folder, eeg_log_folder,
                                            autoreject = True, plot_psd = True, save_psd_plots = True)

#CALCULATING EVOKEDS
reduce_markers = markers.reduce_markers()
evoked_labels = markers.evoked_labels()

evoked_log = my.eeg_prep.calculate_evoked(subjects, path, epochs_clean_folder, evokeds_folder, eeg_log_folder,
                                          reduce_markers, evoked_labels, equalize = False)

#%% ERP CALCULATING AND PLOTTING

#evokeds_folder = 'Results\\EEG\\evokeds\\'

evokeds_labels = markers.evoked_labels()[0]

erp = my.erps.Erp_waveform(subjects, path, evokeds_folder, evokeds_labels)

plot_folder = erps_folder + 'avg_all\\'
erp_labels = markers.erp_labels()

erp_name = 'all'
erp.mean_topo(plot_folder, erp_name, erp_labels, 0.06, 0.6, time_step = 0.06, volt_min = -9, volt_max = 9,
              save_format = 'png')

clusters = {'frontal':['F1', 'Fz', 'F2', 'FC1', 'FCz', 'FC2'],
            'parietal':['CP1', 'CPz', 'CP2', 'P1', 'Pz', 'P2'],
            'temporal':['P7', 'P8', 'PO7', 'PO8', 'P9', 'P10'],
            'occipital':['PO3', 'POz', 'PO4', 'O1', 'Oz', 'O2']}

for roi in clusters:
    erp.comp_plot (plot_folder, roi, erp_labels, clusters[roi], colors = ['#3e8dbfff'], legend = [], save_format = 'png')
    
    
central_comps = OrderedDict({'P1':{'folder':erps_folder + 'P1\\', 'channels':clusters['temporal'], 
                                   'time_window':[80, 120]}, 
                             
                             'N170':{'folder':erps_folder + 'N170\\', 'channels':clusters['temporal'], 
                                     'time_window':[150, 190]}, 
                          
                             'P2':{'folder':erps_folder + 'P2\\', 'channels':clusters['occipital'], 
                                   'time_window':[200, 250]},
                             
                             'N2':{'folder':erps_folder + 'N2\\', 'channels':clusters['frontal'], 
                                   'time_window':[200, 280]},
                        
                             'EPN':{'folder':erps_folder + 'EPN\\', 'channels':clusters['temporal'], 
                                   'time_window':[250, 350]},
                        
                             'P3':{'folder':erps_folder + 'P3b\\', 'channels':clusters['parietal'],
                                    'time_window':[350, 650]}})

lateral_comps = OrderedDict({'N2pc':{'folder':erps_folder  + 'N2pc\\', 'left_channels':['P7','P9', 'PO7'],
                                     'right_channels':['P8','P10', 'PO8'], 'time_window':[250, 350]},
                          
                             'SPCN':{'folder':erps_folder  + 'SPCN\\', 'left_channels':['P7','P9', 'PO7'],
                                     'right_channels':['P8','P10', 'PO8'], 'time_window':[350, 550]}})   


comp_by = {'masking':{'masked':'masked', 'unmasked':'unmasked'}}
contrast = {'faces':{'fearful':['F-F'], 'neutral':['N-N']}, 'task':{'DP':['DP'], 'ID':['ID']}}
erp_labels = markers.erp_labels(comp_by, contrast)
legend = list(erp_labels.keys())
colors = ['#053163ff','#053163ff','#d6614eff','#d6614eff']
styles = ['-', '--', '-', '--']

for comp in central_comps:
    erp.calc_central_erp_amp(central_comps[comp]['folder'], comp, erp_labels, central_comps[comp]['channels'], 
                             central_comps[comp]['time_window']) 
    erp.comp_plot (central_comps[comp]['folder'] + 'plots\\', comp, erp_labels, central_comps[comp]['channels'], 
                   plot_time_wind = True, time_wind = central_comps[comp]['time_window'],
                   colors = colors, styles = styles, legend = legend, save_format = 'png')


contrast_left = {'task':{'DP':['DP'], 'ID':['ID']}, 'faces':{'':['F-N']}}
contrast_right = {'task':{'DP':['DP'], 'ID':['ID']}, 'faces':{'':['N-F']}}
erp_labels_left = markers.erp_labels(comp_by, contrast_left)
erp_labels_right = markers.erp_labels(comp_by, contrast_right)

legend = list(erp_labels_left.keys())
styles = ['--', '-', '--', '-']

for comp in lateral_comps:
    erp.calc_lateral_erp_amp (lateral_comps[comp]['folder'], comp, erp_labels_left, erp_labels_right, 
                              lateral_comps[comp]['left_channels'], lateral_comps[comp]['right_channels'], 
                              lateral_comps[comp]['time_window'])
    erp.lat_comp_plot(lateral_comps[comp]['folder'] + 'plots\\', comp, erp_labels_left, erp_labels_right, 
                      lateral_comps[comp]['left_channels'], lateral_comps[comp]['right_channels'], plot_time_wind = True,
                      time_wind = lateral_comps[comp]['time_window'],
                      colors = colors, styles = styles, legend = legend, save_format = 'png')


#%% EXTRACT COMPONENTS TRIAL BY TRIAL 

reduce_markers = markers.reduce_markers()
evoked_labels = markers.evoked_labels()
epochs_data = my.comps_long.Trials_data(subjects, path, epochs_clean_folder, reduce_markers, evoked_labels = evoked_labels)


conditions = {'task':{'ID':['ID'], 'DP':['DP']}, 'masking':{'masked':['masked'], 'unmasked':['unmasked']}, 
              'faces':{'neutral':['N-N'],'fearful':['F-F']}}
long_labels = markers.comp_long_labels(conditions)

for comp in central_comps:
     epochs_data.get_central_comp(comps_long_folder, comp, long_labels, central_comps[comp]['channels'], 
                                  central_comps[comp]['time_window'])


conditions_left = {'task':{'ID':['ID'], 'DP':['DP']}, 'masking':{'masked':['masked'], 'unmasked':['unmasked']}, 
                   'faces':{'left':['F-N']}}
conditions_right = {'task':{'ID':['ID'], 'DP':['DP']}, 'masking':{'masked':['masked'], 'unmasked':['unmasked']}, 
                    'faces':{'right':['N-F']}}
long_labels_left = markers.comp_long_labels(conditions_left)
long_labels_right = markers.comp_long_labels(conditions_right)

for comp in lateral_comps:
     epochs_data.get_lateral_comp(comps_long_folder, comp, long_labels_left, long_labels_right, lateral_comps[comp]['left_channels'], 
                                  lateral_comps[comp]['right_channels'], lateral_comps[comp]['time_window'])