#-------------------------------------------------------
# Library makefile
#-------------------------------------------------------

version = production
CC =  wcc386 /fpi87 /sg /ot /zq
FOR = wfc386 /fpi87 /sg /ot /quiet /save

version = debugging
CC =  wcc386 /fpi87 /sg /ot /zq
FOR = wfc386 /fpi87 /sg /ot /quiet /save /trace

msg_production = Building PRODUCTION version of library...
msg_debugging  = Building DEBUGGING version of library...

object_files=almnac.obj&
             a_over_b.obj&
             a_plus_b.obj&
             a_xfer_b.obj&
             clineq.obj&
             close_fordrive.obj&
             decode_a_noise.obj&
             decode_control_data.obj&
             decode_date.obj&
             decode_file_name.obj&
             decode_ionosphere.obj&
             decode_list_flt.obj&
             decode_list_int.obj&
             decode_list_str.obj&
             decode_map_area.obj&
             decode_op_area.obj&
             decode_plotter.obj&
             decode_rx_data.obj&
             decode_time.obj&
             decode_tx_data.obj&
             erfc.obj&
             flush_unit.obj&
             gcdbr.obj&
             gcdbr2.obj&
             geo_border.obj&
             geo_chi_map.obj&
             geo_cntr.obj&
             geo_coast_map.obj&
             geo_cond_map.obj&
             geo_coord.obj&
             geo_defs.obj&
             geo_label.obj&
             geo_opa_border.obj&
             geo_terminator.obj&
             get_command_line.obj&
             get_date.obj&
             get_delimiter.obj&
             get_map.obj&
             get_opa.obj&
             get_random.obj&
             get_time.obj&
             get_xmtr.obj&
             grf_begin.obj&
             grf_border.obj&
             grf_cntr.obj&
             grf_cntr_cell.obj&
             grf_cntr_col.obj&
             grf_cntr_edge.obj&
             grf_cntr_fill.obj&
             grf_cntr_flag.obj&
             grf_cntr_holes.obj&
             grf_cntr_join.obj&
             grf_cntr_line.obj&
             grf_cntr_row.obj&
             grf_cntr_search.obj&
             grf_cntr_trace.obj&
             grf_color.obj&
             grf_curve.obj&
             grf_done.obj&
             grf_draw.obj&
             grf_fill_index.obj&
             grf_fill_rect.obj&
             grf_limits.obj&
             grf_move.obj&
             grf_number.obj&
             grf_origin.obj&
             grf_poly_fill.obj&
             grf_string.obj&
             grf_symbol.obj&
             ground.obj&
             hpgl_begin.obj&
             hpgl_color.obj&
             hpgl_done.obj&
             hpgl_draw.obj&
             hpgl_fill_rect.obj&
             hpgl_poly_fill.obj&
             hpgl_string.obj&
             hrs_to_hhmm.obj&
             land_sea.obj&
             leap_year.obj&
             lwpc_dat_loc.obj&
             lwpc_error.obj&
             lwp_driver.obj&
             lwp_eof_mds.obj&
             lwp_get_brng.obj&
             lwp_input.obj&
             lwp_path.obj&
             lwp_preseg.obj&
             lwp_save_mc.obj&
             lwp_set_brng.obj&
             lwp_to_edge.obj&
             lw_fast_mc.obj&
             lw_full_mc.obj&
             lw_htgain.obj&
             lw_read_mds.obj&
             lw_step.obj&
             lw_sum_modes.obj&
             lw_vs_d.obj&
             mc_p_integ.obj&
             mf_bndrys.obj&
             mf_brncpt.obj&
             mf_cheklg.obj&
             mf_driver.obj&
             mf_emtrx.obj&
             mf_fctval.obj&
             mf_fdfdt.obj&
             mf_final.obj&
             mf_finllg.obj&
             mf_fsintg.obj&
             mf_fzeros.obj&
             mf_get_mc.obj&
             mf_initev.obj&
             mf_initlg.obj&
             mf_initoe.obj&
             mf_initr.obj&
             mf_integ.obj&
             mf_introe.obj&
             mf_lagrng.obj&
             mf_lgtori.obj&
             mf_mdhnkl.obj&
             mf_quartc.obj&
             mf_rbars.obj&
             mf_rderiv.obj&
             mf_rmtrx.obj&
             mf_roemtx.obj&
             mf_smtrx.obj&
             mf_subdiv.obj&
             mf_tmtrx.obj&
             mf_todlgs.obj&
             mf_wgsort.obj&
             mf_which.obj&
             mf_wvgd.obj&
             mf_xderiv.obj&
             mf_xfer.obj&
             month_name.obj&
             month_number.obj&
             newmag.obj&
             noise_itsn.obj&
             noise_ntia.obj&
             opa_interpolate.obj&
             opa_noise_grid.obj&
             opa_read_lwf.obj&
             opa_set_up_grid.obj&
             opa_signal_grid.obj&
             open_dat.obj&
             plt_area_size.obj&
             plt_cntr.obj&
             plt_cntr_label.obj&
             plt_cntr_n.obj&
             plt_cntr_range.obj&
             plt_cntr_s.obj&
             plt_cntr_si.obj&
             plt_cntr_sj.obj&
             plt_cntr_sn.obj&
             plt_excd.obj&
             plt_lnp_to_grd.obj&
             plt_lwf.obj&
             plt_lwf_avg.obj&
             plt_lwf_border.obj&
             plt_lwf_label.obj&
             plt_map_indices.obj&
             plt_read_noise.obj&
             plt_read_signal.obj&
             plt_rx_model.obj&
             prfl_chi_exp.obj&
             prfl_chi_tbl.obj&
             prfl_ennu.obj&
             prfl_exp.obj&
             prfl_gtbl.obj&
             prfl_hgts.obj&
             prfl_htbl.obj&
             prfl_init_table.obj&
             prfl_lwpm.obj&
             prfl_read_table.obj&
             prfl_rexp.obj&
             prfl_rtbl.obj&
             prfl_specification.obj&
             prob_to_sigma.obj&
             read_c_grd.obj&
             read_grd.obj&
             read_hdr.obj&
             read_lwf.obj&
             read_mds.obj&
             recvr2.obj&
             set_file_id.obj&
             shift_array.obj&
             shift_array2.obj&
             sortr.obj&
             str_count_list.obj&
             str_get_item.obj&
             str_length.obj&
             str_lower.obj&
             str_trim_char.obj&
             str_upper.obj&
             sw_drvequ.obj&
             sw_extrap.obj&
             sw_fctval.obj&
             sw_initlr.obj&
             sw_integ.obj&
             sw_iterat.obj&
             sw_next.obj&
             sw_quartc.obj&
             sw_rbars.obj&
             sw_rplynm.obj&
             sw_smatrx.obj&
             sw_step.obj&
             sw_uspoly.obj&
             sw_wvgd.obj&
             sw_xsave.obj&
             wf_bndy.obj&
             wf_driver.obj&
             wf_f_fnctn.obj&
             wf_htgain.obj&
             wf_init.obj&
             wf_integ.obj&
             wf_itrate.obj&
             wf_load.obj&
             wf_ma2.obj&
             wf_p_deriv.obj&
             wf_quartc.obj&
             wf_rbars.obj&
             wf_r_mtrx.obj&
             wf_scale.obj&
             wf_step.obj&
             wf_store.obj&
             wf_ti_mtrx.obj&
             wf_t_mtrx.obj&
             wf_xfer.obj&
             world_map.obj&
             write_grd.obj&
             write_hdr.obj&
             write_lwf.obj&
             write_mds.obj&
             xc_add.obj&
             xc_mdhnkl.obj&
             xc_norme.obj&
             xlineq.obj

..\lwpc.lib : $(object_files)
   wlib @librspns

almnac.obj : almnac.for
   $(FOR) almnac

a_over_b.obj : a_over_b.for
   $(FOR) a_over_b

a_plus_b.obj : a_plus_b.for
   $(FOR) a_plus_b

a_xfer_b.obj : a_xfer_b.for
   $(FOR) a_xfer_b

clineq.obj : clineq.for
   $(FOR) clineq

close_fordrive.obj : close_fordrive.for
   $(FOR) close_fordrive

decode_a_noise.obj : decode_a_noise.for
   $(FOR) decode_a_noise

decode_control_data.obj : decode_control_data.for
   $(FOR) decode_control_data

decode_date.obj : decode_date.for
   $(FOR) decode_date

decode_file_name.obj : decode_file_name.for
   $(FOR) decode_file_name

decode_ionosphere.obj : decode_ionosphere.for
   $(FOR) decode_ionosphere

decode_list_flt.obj : decode_list_flt.for
   $(FOR) decode_list_flt

decode_list_int.obj : decode_list_int.for
   $(FOR) decode_list_int

decode_list_str.obj : decode_list_str.for
   $(FOR) decode_list_str

decode_map_area.obj : decode_map_area.for
   $(FOR) decode_map_area

decode_op_area.obj : decode_op_area.for
   $(FOR) decode_op_area

decode_plotter.obj : decode_plotter.for
   $(FOR) decode_plotter

decode_rx_data.obj : decode_rx_data.for
   $(FOR) decode_rx_data

decode_time.obj : decode_time.for
   $(FOR) decode_time

decode_tx_data.obj : decode_tx_data.for
   $(FOR) decode_tx_data

erfc.obj : erfc.for
   $(FOR) erfc

flush_unit.obj : flush_unit.for
   $(FOR) flush_unit

gcdbr.obj : gcdbr.for
   $(FOR) gcdbr

gcdbr2.obj : gcdbr2.for
   $(FOR) gcdbr2

geo_border.obj : geo_border.for
   $(FOR) geo_border

geo_chi_map.obj : geo_chi_map.for
   $(FOR) geo_chi_map

geo_cntr.obj : geo_cntr.for
   $(FOR) geo_cntr

geo_coast_map.obj : geo_coast_map.for
   $(FOR) geo_coast_map

geo_cond_map.obj : geo_cond_map.for
   $(FOR) geo_cond_map

geo_coord.obj : geo_coord.for
   $(FOR) geo_coord

geo_defs.obj : geo_defs.for
   $(FOR) geo_defs

geo_label.obj : geo_label.for
   $(FOR) geo_label

geo_opa_border.obj : geo_opa_border.for
   $(FOR) geo_opa_border

geo_terminator.obj : geo_terminator.for
   $(FOR) geo_terminator

get_command_line.obj : get_command_line.for
   $(FOR) get_command_line

get_date.obj : get_date.for
   $(FOR) get_date

get_delimiter.obj : get_delimiter.for
   $(FOR) get_delimiter

get_map.obj : get_map.for
   $(FOR) get_map

get_opa.obj : get_opa.for
   $(FOR) get_opa

get_random.obj : get_random.for
   $(FOR) get_random

get_time.obj : get_time.for
   $(FOR) get_time

get_xmtr.obj : get_xmtr.for
   $(FOR) get_xmtr

grf_begin.obj : grf_begin.for
   $(FOR) grf_begin

grf_border.obj : grf_border.for
   $(FOR) grf_border

grf_cntr.obj : grf_cntr.for
   $(FOR) grf_cntr

grf_cntr_cell.obj : grf_cntr_cell.for
   $(FOR) grf_cntr_cell

grf_cntr_col.obj : grf_cntr_col.for
   $(FOR) grf_cntr_col

grf_cntr_edge.obj : grf_cntr_edge.for
   $(FOR) grf_cntr_edge

grf_cntr_fill.obj : grf_cntr_fill.for
   $(FOR) grf_cntr_fill

grf_cntr_flag.obj : grf_cntr_flag.for
   $(FOR) grf_cntr_flag

grf_cntr_holes.obj : grf_cntr_holes.for
   $(FOR) grf_cntr_holes

grf_cntr_join.obj : grf_cntr_join.for
   $(FOR) grf_cntr_join

grf_cntr_line.obj : grf_cntr_line.for
   $(FOR) grf_cntr_line

grf_cntr_row.obj : grf_cntr_row.for
   $(FOR) grf_cntr_row

grf_cntr_search.obj : grf_cntr_search.for
   $(FOR) grf_cntr_search

grf_cntr_trace.obj : grf_cntr_trace.for
   $(FOR) grf_cntr_trace

grf_color.obj : grf_color.for
   $(FOR) grf_color

grf_curve.obj : grf_curve.for
   $(FOR) grf_curve

grf_done.obj : grf_done.for
   $(FOR) grf_done

grf_draw.obj : grf_draw.for
   $(FOR) grf_draw

grf_fill_index.obj : grf_fill_index.for
   $(FOR) grf_fill_index

grf_fill_rect.obj : grf_fill_rect.for
   $(FOR) grf_fill_rect

grf_limits.obj : grf_limits.for
   $(FOR) grf_limits

grf_move.obj : grf_move.for
   $(FOR) grf_move

grf_number.obj : grf_number.for
   $(FOR) grf_number

grf_origin.obj : grf_origin.for
   $(FOR) grf_origin

grf_poly_fill.obj : grf_poly_fill.for
   $(FOR) grf_poly_fill

grf_string.obj : grf_string.for
   $(FOR) grf_string

grf_symbol.obj : grf_symbol.for
   $(FOR) grf_symbol

ground.obj : ground.for
   $(FOR) ground

hpgl_begin.obj : hpgl_begin.for
   $(FOR) hpgl_begin

hpgl_color.obj : hpgl_color.for
   $(FOR) hpgl_color

hpgl_done.obj : hpgl_done.for
   $(FOR) hpgl_done

hpgl_draw.obj : hpgl_draw.for
   $(FOR) hpgl_draw

hpgl_fill_rect.obj : hpgl_fill_rect.for
   $(FOR) hpgl_fill_rect

hpgl_poly_fill.obj : hpgl_poly_fill.for
   $(FOR) hpgl_poly_fill

hpgl_string.obj : hpgl_string.for
   $(FOR) hpgl_string

hrs_to_hhmm.obj : hrs_to_hhmm.for
   $(FOR) hrs_to_hhmm

land_sea.obj : land_sea.for
   $(FOR) land_sea

leap_year.obj : leap_year.for
   $(FOR) leap_year

lwpc_dat_loc.obj : lwpc_dat_loc.for
   $(FOR) lwpc_dat_loc

lwpc_error.obj : lwpc_error.for
   $(FOR) lwpc_error

lwp_driver.obj : lwp_driver.for
   $(FOR) lwp_driver

lwp_eof_mds.obj : lwp_eof_mds.for
   $(FOR) lwp_eof_mds

lwp_get_brng.obj : lwp_get_brng.for
   $(FOR) lwp_get_brng

lwp_input.obj : lwp_input.for
   $(FOR) lwp_input

lwp_path.obj : lwp_path.for
   $(FOR) lwp_path

lwp_preseg.obj : lwp_preseg.for
   $(FOR) lwp_preseg

lwp_save_mc.obj : lwp_save_mc.for
   $(FOR) lwp_save_mc

lwp_set_brng.obj : lwp_set_brng.for
   $(FOR) lwp_set_brng

lwp_to_edge.obj : lwp_to_edge.for
   $(FOR) lwp_to_edge

lw_fast_mc.obj : lw_fast_mc.for
   $(FOR) lw_fast_mc

lw_full_mc.obj : lw_full_mc.for
   $(FOR) lw_full_mc

lw_htgain.obj : lw_htgain.for
   $(FOR) lw_htgain

lw_read_mds.obj : lw_read_mds.for
   $(FOR) lw_read_mds

lw_step.obj : lw_step.for
   $(FOR) lw_step

lw_sum_modes.obj : lw_sum_modes.for
   $(FOR) lw_sum_modes

lw_vs_d.obj : lw_vs_d.for
   $(FOR) lw_vs_d

mc_p_integ.obj : mc_p_integ.for
   $(FOR) mc_p_integ

mf_bndrys.obj : mf_bndrys.for
   $(FOR) mf_bndrys

mf_brncpt.obj : mf_brncpt.for
   $(FOR) mf_brncpt

mf_cheklg.obj : mf_cheklg.for
   $(FOR) mf_cheklg

mf_driver.obj : mf_driver.for
   $(FOR) mf_driver

mf_emtrx.obj : mf_emtrx.for
   $(FOR) mf_emtrx

mf_fctval.obj : mf_fctval.for
   $(FOR) mf_fctval

mf_fdfdt.obj : mf_fdfdt.for
   $(FOR) mf_fdfdt

mf_final.obj : mf_final.for
   $(FOR) mf_final

mf_finllg.obj : mf_finllg.for
   $(FOR) mf_finllg

mf_fsintg.obj : mf_fsintg.for
   $(FOR) mf_fsintg

mf_fzeros.obj : mf_fzeros.for
   $(FOR) mf_fzeros

mf_get_mc.obj : mf_get_mc.for
   $(FOR) mf_get_mc

mf_initev.obj : mf_initev.for
   $(FOR) mf_initev

mf_initlg.obj : mf_initlg.for
   $(FOR) mf_initlg

mf_initoe.obj : mf_initoe.for
   $(FOR) mf_initoe

mf_initr.obj : mf_initr.for
   $(FOR) mf_initr

mf_integ.obj : mf_integ.for
   $(FOR) mf_integ

mf_introe.obj : mf_introe.for
   $(FOR) mf_introe

mf_lagrng.obj : mf_lagrng.for
   $(FOR) mf_lagrng

mf_lgtori.obj : mf_lgtori.for
   $(FOR) mf_lgtori

mf_mdhnkl.obj : mf_mdhnkl.for
   $(FOR) mf_mdhnkl

mf_quartc.obj : mf_quartc.for
   $(FOR) mf_quartc

mf_rbars.obj : mf_rbars.for
   $(FOR) mf_rbars

mf_rderiv.obj : mf_rderiv.for
   $(FOR) mf_rderiv

mf_rmtrx.obj : mf_rmtrx.for
   $(FOR) mf_rmtrx

mf_roemtx.obj : mf_roemtx.for
   $(FOR) mf_roemtx

mf_smtrx.obj : mf_smtrx.for
   $(FOR) mf_smtrx

mf_subdiv.obj : mf_subdiv.for
   $(FOR) mf_subdiv

mf_tmtrx.obj : mf_tmtrx.for
   $(FOR) mf_tmtrx

mf_todlgs.obj : mf_todlgs.for
   $(FOR) mf_todlgs

mf_wgsort.obj : mf_wgsort.for
   $(FOR) mf_wgsort

mf_which.obj : mf_which.for
   $(FOR) mf_which

mf_wvgd.obj : mf_wvgd.for
   $(FOR) mf_wvgd

mf_xderiv.obj : mf_xderiv.for
   $(FOR) mf_xderiv

mf_xfer.obj : mf_xfer.for
   $(FOR) mf_xfer

month_name.obj : month_name.for
   $(FOR) month_name

month_number.obj : month_number.for
   $(FOR) month_number

newmag.obj : newmag.for
   $(FOR) newmag

noise_itsn.obj : noise_itsn.for
   $(FOR) noise_itsn

noise_ntia.obj : noise_ntia.for
   $(FOR) noise_ntia

opa_interpolate.obj : opa_interpolate.for
   $(FOR) opa_interpolate

opa_noise_grid.obj : opa_noise_grid.for
   $(FOR) opa_noise_grid

opa_read_lwf.obj : opa_read_lwf.for
   $(FOR) opa_read_lwf

opa_set_up_grid.obj : opa_set_up_grid.for
   $(FOR) opa_set_up_grid

opa_signal_grid.obj : opa_signal_grid.for
   $(FOR) opa_signal_grid

open_dat.obj : open_dat.for
   $(FOR) open_dat

plt_area_size.obj : plt_area_size.for
   $(FOR) plt_area_size

plt_cntr.obj : plt_cntr.for
   $(FOR) plt_cntr

plt_cntr_label.obj : plt_cntr_label.for
   $(FOR) plt_cntr_label

plt_cntr_n.obj : plt_cntr_n.for
   $(FOR) plt_cntr_n

plt_cntr_range.obj : plt_cntr_range.for
   $(FOR) plt_cntr_range

plt_cntr_s.obj : plt_cntr_s.for
   $(FOR) plt_cntr_s

plt_cntr_si.obj : plt_cntr_si.for
   $(FOR) plt_cntr_si

plt_cntr_sj.obj : plt_cntr_sj.for
   $(FOR) plt_cntr_sj

plt_cntr_sn.obj : plt_cntr_sn.for
   $(FOR) plt_cntr_sn

plt_excd.obj : plt_excd.for
   $(FOR) plt_excd

plt_joint_cntr.obj : plt_joint_cntr.for
   $(FOR) plt_joint_cntr

plt_joint_label.obj : plt_joint_label.for
   $(FOR) plt_joint_label

plt_lnp_to_grd.obj : plt_lnp_to_grd.for
   $(FOR) plt_lnp_to_grd

plt_lwf.obj : plt_lwf.for
   $(FOR) plt_lwf

plt_lwf_avg.obj : plt_lwf_avg.for
   $(FOR) plt_lwf_avg

plt_lwf_border.obj : plt_lwf_border.for
   $(FOR) plt_lwf_border

plt_lwf_label.obj : plt_lwf_label.for
   $(FOR) plt_lwf_label

plt_map_indices.obj : plt_map_indices.for
   $(FOR) plt_map_indices

plt_read_noise.obj : plt_read_noise.for
   $(FOR) plt_read_noise

plt_read_signal.obj : plt_read_signal.for
   $(FOR) plt_read_signal

plt_rx_model.obj : plt_rx_model.for
   $(FOR) plt_rx_model

prfl_chi_exp.obj : prfl_chi_exp.for
   $(FOR) prfl_chi_exp

prfl_chi_tbl.obj : prfl_chi_tbl.for
   $(FOR) prfl_chi_tbl

prfl_ennu.obj : prfl_ennu.for
   $(FOR) prfl_ennu

prfl_exp.obj : prfl_exp.for
   $(FOR) prfl_exp

prfl_gtbl.obj : prfl_gtbl.for
   $(FOR) prfl_gtbl

prfl_hgts.obj : prfl_hgts.for
   $(FOR) prfl_hgts

prfl_htbl.obj : prfl_htbl.for
   $(FOR) prfl_htbl

prfl_init_table.obj : prfl_init_table.for
   $(FOR) prfl_init_table

prfl_lwpm.obj : prfl_lwpm.for
   $(FOR) prfl_lwpm

prfl_read_table.obj : prfl_read_table.for
   $(FOR) prfl_read_table

prfl_rexp.obj : prfl_rexp.for
   $(FOR) prfl_rexp

prfl_rtbl.obj : prfl_rtbl.for
   $(FOR) prfl_rtbl

prfl_specification.obj : prfl_specification.for
   $(FOR) prfl_specification

prob_to_sigma.obj : prob_to_sigma.for
   $(FOR) prob_to_sigma

read_c_grd.obj : read_c_grd.for
   $(FOR) read_c_grd

read_grd.obj : read_grd.for
   $(FOR) read_grd

read_hdr.obj : read_hdr.for
   $(FOR) read_hdr

read_lwf.obj : read_lwf.for
   $(FOR) read_lwf

read_mds.obj : read_mds.for
   $(FOR) read_mds

recvr2.obj : recvr2.for
   $(FOR) recvr2

set_file_id.obj : set_file_id.for
   $(FOR) set_file_id

shift_array.obj : shift_array.for
   $(FOR) shift_array

shift_array2.obj : shift_array2.for
   $(FOR) shift_array2

sortr.obj : sortr.for
   $(FOR) sortr

str_count_list.obj : str_count_list.for
   $(FOR) str_count_list

str_get_item.obj : str_get_item.for
   $(FOR) str_get_item

str_length.obj : str_length.for
   $(FOR) str_length

str_lower.obj : str_lower.for
   $(FOR) str_lower

str_trim_char.obj : str_trim_char.for
   $(FOR) str_trim_char

str_upper.obj : str_upper.for
   $(FOR) str_upper

sw_drvequ.obj : sw_drvequ.for
   $(FOR) sw_drvequ

sw_extrap.obj : sw_extrap.for
   $(FOR) sw_extrap

sw_fctval.obj : sw_fctval.for
   $(FOR) sw_fctval

sw_initlr.obj : sw_initlr.for
   $(FOR) sw_initlr

sw_integ.obj : sw_integ.for
   $(FOR) sw_integ

sw_iterat.obj : sw_iterat.for
   $(FOR) sw_iterat

sw_next.obj : sw_next.for
   $(FOR) sw_next

sw_quartc.obj : sw_quartc.for
   $(FOR) sw_quartc

sw_rbars.obj : sw_rbars.for
   $(FOR) sw_rbars

sw_rplynm.obj : sw_rplynm.for
   $(FOR) sw_rplynm

sw_smatrx.obj : sw_smatrx.for
   $(FOR) sw_smatrx

sw_step.obj : sw_step.for
   $(FOR) sw_step

sw_uspoly.obj : sw_uspoly.for
   $(FOR) sw_uspoly

sw_wvgd.obj : sw_wvgd.for
   $(FOR) sw_wvgd

sw_xsave.obj : sw_xsave.for
   $(FOR) sw_xsave

wf_bndy.obj : wf_bndy.for
   $(FOR) wf_bndy

wf_driver.obj : wf_driver.for
   $(FOR) wf_driver

wf_f_fnctn.obj : wf_f_fnctn.for
   $(FOR) wf_f_fnctn

wf_htgain.obj : wf_htgain.for
   $(FOR) wf_htgain

wf_init.obj : wf_init.for
   $(FOR) wf_init

wf_integ.obj : wf_integ.for
   $(FOR) wf_integ

wf_itrate.obj : wf_itrate.for
   $(FOR) wf_itrate

wf_load.obj : wf_load.for
   $(FOR) wf_load

wf_ma2.obj : wf_ma2.for
   $(FOR) wf_ma2

wf_p_deriv.obj : wf_p_deriv.for
   $(FOR) wf_p_deriv

wf_quartc.obj : wf_quartc.for
   $(FOR) wf_quartc

wf_rbars.obj : wf_rbars.for
   $(FOR) wf_rbars

wf_r_mtrx.obj : wf_r_mtrx.for
   $(FOR) wf_r_mtrx

wf_scale.obj : wf_scale.for
   $(FOR) wf_scale

wf_step.obj : wf_step.for
   $(FOR) wf_step

wf_store.obj : wf_store.for
   $(FOR) wf_store

wf_ti_mtrx.obj : wf_ti_mtrx.for
   $(FOR) wf_ti_mtrx

wf_t_mtrx.obj : wf_t_mtrx.for
   $(FOR) wf_t_mtrx

wf_xfer.obj : wf_xfer.for
   $(FOR) wf_xfer

world_map.obj : world_map.for
   $(FOR) world_map

write_grd.obj : write_grd.for
   $(FOR) write_grd

write_hdr.obj : write_hdr.for
   $(FOR) write_hdr

write_lwf.obj : write_lwf.for
   $(FOR) write_lwf

write_mds.obj : write_mds.for
   $(FOR) write_mds

xc_add.obj : xc_add.for
   $(FOR) xc_add

xc_mdhnkl.obj : xc_mdhnkl.for
   $(FOR) xc_mdhnkl

xc_norme.obj : xc_norme.for
   $(FOR) xc_norme

xlineq.obj : xlineq.for
   $(FOR) xlineq

