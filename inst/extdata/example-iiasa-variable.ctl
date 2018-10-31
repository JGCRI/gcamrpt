GCAM variable,output variable,aggregation keys,aggregation function,years,filters,filter_operator,output units
Population,Population,,,2000:2050,,,thous
pcGDP(PPP),GDP|PPP,,,2000:2050,,,Thous80US$/per
Electricity,Electricity|Generation,,,2000:2050,(matches; sector; electricity),,MWh
Electricity,Electricity|Total,,,2000:2050,,,
Electricity,Electricity|Rooftop PV,,,2000:2050,(matches; sector; elect_td_bld),,MWh
Electricity,Electricity|Rooftop PV|Ridiculous,,,2000:2050,"(notmatches; sector; electricity), (notmatches; sector; industrial energy use)",,MWh
air_pollution,Emissions|BC|Energy|Demand|Transportation,,,2000:2050,"(matches; subsector; Domestic Aviation),(matches; subsector; International Aviation)","OR",Tg
