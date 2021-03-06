begin
  start_write_time = get_cpu_time()

  DateYMD = systemfunc("date '+%m%d%Y'")

;---Set some NetCDF options before we open the file
  setfileoption("nc","preFill",False)
  setfileoption("nc","defineMode",True)
;---Open a new NetCDF file to write to
  fout_name="jarmodel.cdomcnp_pars."+DateYMD+".nc"
  system("rm -f " + fout_name)
  fout=addfile(fout_name, "c")
;---create dimensions
  allpfts          = ispan(1,1,1)
  allpfts!0        ="allpfts"
  allpfts@long_nmae="allpft index"
  allpfts@units    ="none"

  pfts          = ispan(1,25,1)
  pfts!0        ="pfts"
  pfts@long_nmae="pft index"
  pfts@units    ="none"


  surfsoil          = ispan(1,2,1)
  surfsoil!0        ="surfsoil"
  surfsoil@long_name="at surface or in soil index"
  surfsoil@units    = "none"

  dims=dimsizes(allpfts)
  dim_names=(/allpfts!0/)
  dim_unlimited=(/False/)
  filedimdef(fout,dim_names,dims,dim_unlimited)
  delete(dims)
  dims=dimsizes(surfsoil)
  dim_names=(/surfsoil!0/)
  dim_unlimited=(/False/)
  filedimdef(fout,dim_names,dims,dim_unlimited)

  delete(dims)
  dims=dimsizes(pfts)
  dim_names=(/pfts!0/)
  dim_unlimited=(/False/)
  filedimdef(fout,dim_names,dims,dim_unlimited)

;---define varaible
  filevardef(fout,"cwd_fcel","float",(/allpfts!0/))
  filevardef(fout,"cwd_flig","float",(/allpfts!0/))
  filevardef(fout,"fwd_fcel","float",(/allpfts!0/))
  filevardef(fout,"fwd_flig","float",(/allpfts!0/))
  filevardef(fout,"lwd_fcel","float",(/allpfts!0/))
  filevardef(fout,"lwd_flig","float",(/allpfts!0/))
  filevardef(fout,"surface_tension_water","float",(/allpfts!0/))
  filevardef(fout,"rij_kro_a","float",(/allpfts!0/))
  filevardef(fout,"rij_kro_alpha","float",(/allpfts!0/))
  filevardef(fout,"rij_kro_beta","float",(/allpfts!0/))
  filevardef(fout,"rij_kro_gamma","float",(/allpfts!0/))
  filevardef(fout,"rij_kro_delta","float",(/allpfts!0/))
  filevardef(fout,"minpsi_bgc","float",(/allpfts!0/))
  filevardef(fout,"k_nitr_max","float",(/allpfts!0/))
  filevardef(fout,"Q10","float",(/allpfts!0/))
  filevardef(fout,"froz_q10","float",(/allpfts!0/))
  filevardef(fout,"decomp_depth_efolding","float",(/allpfts!0/))
  filevardef(fout,"rf_l3s2_bgc","float",(/allpfts!0/))
  filevardef(fout,"rf_s2s1_bgc","float",(/allpfts!0/))
  filevardef(fout,"rf_s3s1_bgc","float",(/allpfts!0/))
  filevardef(fout,"k_decay_pom","float",(/allpfts!0/))
  filevardef(fout,"k_decay_humus","float",(/allpfts!0/))
  filevardef(fout,"k_decay_cwd","float",(/allpfts!0/))
  filevardef(fout,"k_decay_lwd","float",(/allpfts!0/))
  filevardef(fout,"k_decay_fwd","float",(/allpfts!0/))
  filevardef(fout,"k_m_o2_bgc","float",(/allpfts!0/))
  filevardef(fout,"organic_max","float",(/allpfts!0/))
  filevardef(fout,"nitrif_n2o_loss_frac","float",(/allpfts!0/))
  filevardef(fout,"rf_l1s1_bgc","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"rf_l2s1_bgc","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"rf_s1s2a_bgc","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"rf_s1s2b_bgc","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"k_decay_lmet","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"k_decay_lcel","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"k_decay_llig","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"k_decay_mic","float",(/allpfts!0,surfsoil!0/))
  filevardef(fout,"vmax_minp_soluble_to_secondary","float",(/pfts!0/))
  filevardef(fout,"frac_p_sec_to_sol","float",(/pfts!0/))
  filevardef(fout,"minp_secondary_decay","float",(/pfts!0/))
  filevardef(fout,"k_decay_dom","float",(/allpfts!0/))
  filevardef(fout,"km_mic_som","float",(/allpfts!0/))



;---define variable attributes

   fatt=True
   fatt@long_name="fraction of cellulose carbon in coarse woody roots"
   fatt@units = "none"
   filevarattdef(fout,"cwd_fcel", fatt)

   fatt@long_name="fraction of lignin carbon in coarse woody roots"
   fatt@units = "none"
   filevarattdef(fout,"cwd_flig", fatt)

   fatt@long_name="fraction of cellulose carbon in fine woody branches"
   fatt@units = "none"
   filevarattdef(fout,"fwd_fcel", fatt)

   fatt@long_name="fraction of lignin carbon in fine woody branches"
   fatt@units = "none"
   filevarattdef(fout,"fwd_flig", fatt)

   fatt@long_name="fraction of cellulose carbon in large woody branches"
   fatt@units = "none"
   filevarattdef(fout,"lwd_fcel", fatt)

   fatt@long_name="fraction of lignin carbon in large woody branches"
   fatt@units = "none"
   filevarattdef(fout,"lwd_flig", fatt)

   fatt@long_name="surface tension of water"
   fatt@units = "N m-1"
   filevarattdef(fout,"surface_tension_water", fatt)

   fatt@long_name="parameter a for arah-vinten anaerobic fraction"
   fatt@units = "none"
   filevarattdef(fout,"rij_kro_a", fatt)

   fatt@long_name="parameter alpha for arah-vinten anaerobic fraction"
   fatt@units = "none"
   filevarattdef(fout,"rij_kro_alpha", fatt)

   fatt@long_name="parameter beta for arah-vinten anaerobic fraction"
   fatt@units = "none"
   filevarattdef(fout,"rij_kro_beta", fatt)

   fatt@long_name="parameter gamma for arah-vinten anaerobic fraction"
   fatt@units = "none"
   filevarattdef(fout,"rij_kro_gamma", fatt)

   fatt@long_name="parameter delta for arah-vinten anaerobic fraction"
   fatt@units = "none"
   filevarattdef(fout,"rij_kro_delta", fatt)

   fatt@long_name="minimum suction pressure with active aerobic respiration"
   fatt@units = "M Pa"
   filevarattdef(fout,"minpsi_bgc", fatt)

   fatt@long_name="maximum nitrification rate"
   fatt@units = "1/s"
   filevarattdef(fout,"k_nitr_max", fatt)

   fatt@long_name="Increase in reaction rate per 10 K increase in temperature for unfrozen condition"
   fatt@units = "none"
   filevarattdef(fout,"Q10", fatt)

   fatt@long_name="Increase in reaction rate per 10 K increase in temperature for frozen condition"
   fatt@units = "none"
   filevarattdef(fout,"froz_q10", fatt)

   fatt@long_name="The relaxing depth to impose the decrease of respiration along depth"
   fatt@units = "1/m"
   filevarattdef(fout,"decomp_depth_efolding", fatt)

   fatt@long_name="respiraiton efficiency during l3->s2"
   fatt@units = "none"
   filevarattdef(fout,"rf_l3s2_bgc", fatt)

   fatt@long_name="respiration efficiency during s2->s1"
   fatt@units = "none"
   filevarattdef(fout,"rf_s2s1_bgc", fatt)

   fatt@long_name="respiration efficiency during s3->s1"
   fatt@units = "none"
   filevarattdef(fout,"rf_s3s1_bgc", fatt)

   fatt@long_name="intrinsic decay rate of pom"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_pom", fatt)

   fatt@long_name="intrinsic decay rate of humus"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_humus", fatt)

   fatt@long_name="intrinsic decay rate of coarse woody root"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_cwd", fatt)

   fatt@long_name="intrinsic decay rate of large woody debries"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_lwd", fatt)

   fatt@long_name="intrinsic decay rate of fine woody root"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_fwd", fatt)

   fatt@long_name="half saturation constant for MM-based O2 binding"
   fatt@units = "mol/m3"
   filevarattdef(fout,"k_m_o2_bgc", fatt)

   fatt@long_name="maximum density of organic matter"
   fatt@units = "kg m-3"
   filevarattdef(fout,"organic_max", fatt)

   fatt@long_name="respiration efficiency during l1->s1"
   fatt@units = "none"
   filevarattdef(fout,"rf_l1s1_bgc", fatt)

   fatt@long_name="respiration efficiency during l2->s1"
   fatt@units = "none"
   filevarattdef(fout,"rf_l2s1_bgc", fatt)

   fatt@long_name="regression parameter a for computing respiration efficiency during s1->s2"
   fatt@units = "none"
   filevarattdef(fout,"rf_s1s2a_bgc", fatt)

   fatt@long_name="half saturation constatnt for microbial biomass for modeling linear decomposition"
   fatt@units = "mol C m-3"
   filevarattdef(fout,"km_mic_som", fatt)


   fatt@long_name="regression parameter b for computing respiration efficiency during s1->s2"
   fatt@units = "none"
   filevarattdef(fout,"rf_s1s2b_bgc", fatt)

   fatt@long_name="intrinsic decay rate of pool dom"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_dom", fatt)

   fatt@long_name="intrinsic decay rate of pool lit1"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_lmet", fatt)

   fatt@long_name="intrinsic decay rate of pool lit2"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_lcel", fatt)

   fatt@long_name="intrinsic decay rate of pool lit3"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_llig", fatt)

   fatt@long_name="intrinsic decay rate of pool _mic"
   fatt@units = "1/year"
   filevarattdef(fout,"k_decay_mic", fatt)

   fatt@long_name="maximum conversion rate of soprobed inorganic P into secondary P"
   fatt@units = "1/s"
   filevarattdef(fout,"vmax_minp_soluble_to_secondary", fatt)

   fatt@long_name="fraction of secondary inorganic P into soluble P during secondary inorganic P mobilization"
   fatt@units = "none"
   filevarattdef(fout,"frac_p_sec_to_sol", fatt)

   fatt@long_name="secondary inorganic P mobilization rate"
   fatt@units = "1/s"
   filevarattdef(fout,"minp_secondary_decay", fatt)

   fatt@long_name="fraction of nitrification product as n2o"
   fatt@units = "none"
   filevarattdef(fout,"nitrif_n2o_loss_frac", fatt)

;   if (False) then
   fout->cwd_fcel=(/0.76/)
   fout->cwd_flig=(/0.26/)

   fout->fwd_fcel=(/0.78/)
   fout->fwd_flig=(/0.22/)

   fout->lwd_fcel=(/0.76/)
   fout->lwd_flig=(/0.26/)

   fout->surface_tension_water=(/0.073/)
   fout->Q10=(/2.0/)
   fout->froz_q10=(/10.0/)
   fout->decomp_depth_efolding=(/0.7/)
   fout->rf_l1s1_bgc=(/(/0.55/),(/0.55/)/)
   fout->rf_l2s1_bgc=(/(/0.45/),(/0.55/)/)
   fout->rf_l3s2_bgc=(/0.3/)
   fout->rf_s2s1_bgc=(/0.55/)
   fout->rf_s3s1_bgc=(/0.55/)
   fout->rf_s1s2a_bgc=(/(/0.60/),(/0.17/)/)
   fout->rf_s1s2b_bgc=(/(/0.00/),(/0.68/)/)
   fout->k_decay_lmet=(/(/14.8/),(/18.5/)/)
   fout->k_decay_lcel=(/(/3.9/),(/4.9/)/)
   fout->k_decay_llig=(/(/3.9/),(/4.9/)/)
   fout->k_decay_mic=(/(/6.7/),(/7.3/)/)
   fout->k_decay_pom=(/0.2/)
   fout->k_decay_pom=(/0.0045/)
   fout->k_decay_cwd=(/0.6/)
   fout->k_decay_fwd=(/1.5/)
   fout->k_decay_lwd=(/0.5/)
   fout->k_decay_dom=(/8.0/)
   fout->km_mic_som=(/3.0/)

   aa=1.5*10^(-7.48)
   fout->nitrif_n2o_loss_frac=(/1.e-4/)
   fout->organic_max = (/160.0/)
   ;the Arah and Vinten value is 1.8e-4
   fout->rij_kro_a  = (/aa/)
   ;the Arah and Vinten value is 0.77
   fout->rij_kro_alpha=(/1.26/)
   ;the Arah and Vinten value is 0.37
   fout->rij_kro_beta =(/0.6/)
   ;the Arah and Vinten value is 0.32
   fout->rij_kro_gamma = (/0.6/)
   ;the Arah and Vinten value is 0.45
   fout->rij_kro_delta = (/0.85/)

   fout->vmax_minp_soluble_to_secondary=(/1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,\
                                          1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,\
                                          1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,\
                                          1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,1.e-9,\
                                          1.e-9/)
   fout->frac_p_sec_to_sol = (/0.95,0.95,0.95,0.95,0.95,0.95,\
                               0.95,0.95,0.95,0.95,0.95,0.95,\
                               0.95,0.95,0.95,0.95,0.95,0.95,\
                               0.95,0.95,0.95,0.95,0.95,0.95,\
                               0.95/)

   fout->minp_secondary_decay = (/1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,\
                                  1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,\
                                  1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,\
                                  1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,1.e-11,\
                                  1.e-11/)
;   end if
   delete(fout)
   end_write_time = get_cpu_time()

end
