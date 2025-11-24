## code to prepare `morp_groups` dataset goes here
library(crcheck)
library(purrr)
morps <- list(
  m1 = c(gen_morp(8051:8084), gen_morp(8120:8131)),
  m2 = gen_morp(8090:8110),
  m3 = c(gen_morp(8140:8149), gen_morp(8160:8162), gen_morp(8190:8221),
         gen_morp(8260:8337), gen_morp(8350:8551), gen_morp(8570:8576),
         gen_morp(8940:8941)),
  m4 = c(gen_morp(8030:8046), gen_morp(8150:8157), gen_morp(8170:8180),
         gen_morp(8230:8255), gen_morp(8340:8347), gen_morp(8560:8562),
         gen_morp(8580:8671)),
  m5 = c(gen_morp(8010:8015), gen_morp(8020:8022), 8050),
  m6 = c(gen_morp(8680:8713), gen_morp(8800:8921), gen_morp(8990:8991),
         gen_morp(9040:9044), gen_morp(9120:9125), gen_morp(9130:9136),
         gen_morp(9141:9252), gen_morp(9370:9373), gen_morp(9540:9582)),
  m7 = c(gen_morp(9050:9055)),
  m8 = c(9840, gen_morp(9861:9931), gen_morp(9945:9946), 9950,
         gen_morp(9961:9964), gen_morp(9980:9987)),
  m9 = c(gen_morp(9670:9699), 9728, gen_morp(9731:9734), gen_morp(9761:9767),
         9769, gen_morp(9823:9826), 9833, 9836, 9940),
  m10 = c(gen_morp(9700:9719), 9729, 9768, gen_morp(9827:9831), 9834,
          9837, 9948),
  m11 = gen_morp(9650:9667),
  m12 = gen_morp(9740:9742),
  m13 = gen_morp(9750:9758),
  m14 = c(gen_morp(9590:9591), 9596, 9727, 9760, 9800, 9801, 9805, 9820,
          9832, 9835, 9860, 9960, 9970, 9975, 9989),
  m15 = 9140,
  m16 = c(gen_morp(8720:8790), gen_morp(8930:8936), gen_morp(8950:8983),
          gen_morp(9000:9030), gen_morp(9060:9110), gen_morp(9260:9365),
          gen_morp(9380:9539)),
  m17 = gen_morp(8000:8005)
)


morp_groups <- map2(morps, names(morps), ~ {
  setNames(.x, rep(.y, length(.x)))
  }) |> 
  flatten_dbl()

