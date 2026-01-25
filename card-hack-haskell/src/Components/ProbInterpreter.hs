module Components.ProbInterpreter (probAnalise) where

probAnalise :: (Double, Double) -> String
probAnalise (0.00, 0.00) = unlines 
    ["OverFlow da quantidade de pontos do usuário, impossível tomar alguma ação.",
    "Probabilidade ao puxar uma carta: 0.00%",
    "Probabilidade ao manter as cartas: 0.00%"
    ]

probAnalise (x, 0.00) = unlines 
    ["Pontuação inferior que o Dealer, assim: Observa-se que a probabilidade de vencer sem puxar nenhuma carta é nula. Sendo assim, você deve puxar uma carta.",
    "Probabilidade ao puxar uma carta: " ++ (show x) ++ "%",
    "Probabilidade ao manter as cartas: 0.00%"
    ]

probAnalise (0.00, 100.00) = unlines
    ["Você alcançou Blackjack, 21 pontos. Apenas aguarde sua vitória e não puxe nenhuma carta. (espero não ter zicado, pois o Dealer pode conseguir fazer Blackjack também).",
    "Probabilidade ao puxar uma carta: 0.00%",
    "Probabilidade ao manter as cartas: 99.99%"]

probAnalise (getCardProb, stayProb)
    | diff <= 10.00  && diff >= 5.00   =
        unlines
            [ "As probabilidades indicam que existe uma grande semelhança nas probabilidades,"
            , "contudo a probabilidade de puxar uma carta é um pouco melhor:"
            , showProbs
            ]

    | diff >= -10.00 && diff <= -5.00  =
        unlines
            [ "As probabilidades indicam que apresenta uma grande semelhança nas probabilidades,"
            , "contudo a probabilidade de não puxar uma carta (stay) é um pouco melhor:"
            , showProbs
            ]

    | diff > -5.00 && diff < 5.00    =
        unlines
            [ "Praticamente a mesma probabilidade, vai na fé irmão:"
            , showProbs
            ]

    | diff > 10.00                  =
        unlines
            [ "Diferença considerável entre as probabilidades,"
            , "indicando que a melhor ação é puxar uma carta:"
            , showProbs
            ]

    | diff < -10.00                 =
        unlines
            [ "Diferença considerável entre as probabilidades,"
            , "indicando que a melhor ação é não puxar uma carta (stay):"
            , showProbs
            ]
    where
        diff = getCardProb - stayProb

        showProbs =
            unlines
                [ "Probabilidade ao puxar uma carta: " ++ show getCardProb ++ "%"
                , "Probabilidade ao manter as cartas: " ++ show stayProb ++ "%"
                ]
