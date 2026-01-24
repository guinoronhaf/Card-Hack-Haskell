module Components.ProbInterpreter (probAnalise) where

probAnalise :: (Double, Double) -> String
probAnalise (0.00, 0.00) = unlines 
    ["OverFlow da quantidade de pontos do usuário, impossível tomar nenhuma ação.",
    "Probabilidade ao puxar uma carta: 0.0",
    "Probabilidade ao manter as cartas: 0.0"
    ]

probAnalise (x, 0.00) = unlines 
    ["Pontuação inferior que o Dealer, assim: Observa-se que a probabilidade de vencer sem puxar nenhuma carta é nula.",
    "Probabilidade ao puxar uma carta: " ++ (show x),
    "Probabilidade ao manter as cartas: 0.0"
    ]

probAnalise (getCardProb, stayProb)
    | diff <= 5.00  && diff >= 2.00   =
        unlines
            [ "As probabilidades indicam que apresenta uma grande semelhança nas probabilidades,"
            , "contudo a probabilidade de puxar uma carta é um pouco melhor:"
            , showProbs
            ]

    | diff >= -5.00 && diff <= -2.00  =
        unlines
            [ "As probabilidades indicam que apresenta uma grande semelhança nas probabilidades,"
            , "contudo a probabilidade de não puxar uma carta (stay) é um pouco melhor:"
            , showProbs
            ]

    | diff > -2.00 && diff < 2.00    =
        unlines
            [ "Praticamente a mesma probabilidade, vai na fé irmão:"
            , showProbs
            ]

    | diff > 5.00                  =
        unlines
            [ "Diferença considerável entre as probabilidades,"
            , "indicando que a melhor ação é puxar uma carta:"
            , showProbs
            ]

    | diff < -5.00                 =
        unlines
            [ "Diferença considerável entre as probabilidades,"
            , "indicando que a melhor ação é não puxar uma carta (stay):"
            , showProbs
            ]
    where
        diff = getCardProb - stayProb

        showProbs =
            unlines
                [ "Probabilidade ao puxar uma carta: " ++ show getCardProb
                , "Probabilidade ao manter as cartas: " ++ show stayProb
                ]
