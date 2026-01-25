<h1 align="center">Card-Hack-Haskell</h1>

_Blackjack_, também conhecido como "21", é um jogo de cartas bastante popular em cassinos e casas de aposta no mundo todo. Suas regras simples podem esconder sua natureza probabilística interessante. Nesse sentido, _Card-Hack_ implementa em _Haskell_ um algoritmo de probabilidades que determina a melhor jogada possível em um contexto de rodadas de _Blackjack_.

---

## Sumário

 - [Estrutura de diretórios](#estrutura-de-diretórios)
 - [Blackjack: objetivos e movimentos](#blackjack-objetivos-e-movimentos)
 - [Cálculo de probabilidades](#cálculo-de-probabilidades)
 - [Como executar o algoritmo](#como-executar-o-algoritmo)
 - [Contribuidores deste repositório](#contribuidores-deste-repositório)

## Estrutura de diretórios

_Card-Hack_ possui três diretórios principais:


| Diretório | Conteúdo |
| --------- | -------- |
| **app** | Contém o arquivo `Main.hs`, responsável por iniciar a aplicação |
| **src** | Contém módulos (arquivos) principais e auxiliares do algoritmo |
| **test** | Contém testes unitários para a aplicação |


Visualmente, a hierarquia de diretórios tem a seguinte forma:

```diff
card-hack-haskell/
├── app/
│   └── Main.hs
├── src/
│   ├── Components/
│   │   ├── Button.hs
│   │   ├── Deck.hs
│   │   ├── ProbAlgorithm.hs
│   │   └── ProbInterpreter.hs
│   ├── Util/
│   │   ├── AuxiliaryFunctions.hs
│   │   └── Game.hs
│   └── Lib.hs
└── test/
    └── Spec.hs
```

## Blackjack: objetivos e movimentos

Antes de tudo, é preciso compreender quem são as figuras que compõem uma mesa de _Blackjack_. São eles:

 - **Dealer**: representa a casa de apostas (cassino). Joga sempre após o(s) jogador(es) e possui uma vantagem inata em jogos de azar: a chamada _house-edge_.
 - **Jogador(es)**: busca(m), de maneira individual, vencer o dealer. Joga(m) sempre antes do _Dealer_.

Nesse sentido, os objetivos de _Blackjack_ são bem simples e diretos. Em suma, é preciso:

 - Possuir um valor exato de 21 pontos ao somar os valores de suas cartas ("_Blackjack_!") **OU**
 - Possuir um somatório de valor de cartas superior ao do _Dealer_ ao fim de uma rodada **E**
 - Não extrapolar os 21 pontos no somatório.

É importante destacar que uma rodada é constituída pela jogada de todos os jogadores e do _Dealer_.

### Movimentos de Blackjack considerados em _Card-Hack_

_Blackjack_ possui inúmeras variações ao redor do mundo e, consequentemente, diversos movimentos podem ser adicionados para tornar o jogo mais dinâmico ou difícil. No caso de _Card-Hack_, serão considerados apenas 2 movimentos, que podem ser realizados tanto pelo jogador quanto pelo _Dealer_:

 - **Stay**: a pessoa do turno não pega nenhuma carta do baralho e passa a vez;
 - **Hit**: a pessoa do turno puxa uma carta do baralho e verifica as condições de vitória.

## Cálculo de probabilidades

Ao final da execução do algoritmo pelo módulo `ProbAlgorithm.hs`, uma tupla de elemento do tipo _Double_ é retornada, contendo respectivamente: **a probabilidade de o jogador vencer em sua próxima jogada caso opte pelo _hit_** e **a probabilidade de o jogador vencer em sua próxima jogada caso opte pelo _stay_**.

### Vencer com _hit_

Para que o jogador vença com um _hit_ em sua próxima jogada, 2 cenários devem ser considerados:

 - Antes de jogar, o jogador possui **menos** pontos do que o _Dealer_; -> P(menos)
 - Antes de jogar, o jogador possui **mais** pontos do que o _Dealer_. -> P(mais)

Como exemplo para demonstrar os cálculos realizados em `ProbAlgorithm.hs`, consideremos a probabilidade de o jogador vencer em sua próxima jogada caso opte por _hit_ e possua menos pontos do que o _Dealer_. Nesse caso, tem-se:

### Vencer com _stay_

Para que vença o _stay_ em sua próxima jogada, apenas um caso é considerado:

 - Antes de jogar, o jogador possui **mais** pontos do que o _Dealer_.

É válido pontuar que o caso em que o jogador possui **menos** pontos do que o _Dealer_ não é considerado pois, nesse caso, caso o jogador opte por _stay_, basta o _Dealer_ também optar por _stay_, e o jogador será derrotado.

Para exemplificar nossos casos, consideremos então o caso válido. Nesse caso, a probabilidade de o jogador vencer em sua próxima jogada optando por _stay_ é:

**ProbVitoria = Probabilidade de o _Dealer_ pegar uma carta e ficar abAixo da pontuação do jogador**

### Componentes importantes

 - `AuxiliaryFunctions.hs`: módulo auxiliar que contém funções auxiliares para os componentes principais da aplicação;
 - `Deck.hs`: módulo auxiliar que representa um baralho (_deck_) convencional. Fundamental para a contagem de cartas no cálculo das probabiliades;
 - `ProbAlgorith.hs`: módulo lógico que calcula efetivamente todas as probabilidades a partir de funções auxiliares como `underflowProb`, `blackjackProb` e `overlfowProb`;
 - `ProbInterpreter.hs`: módulo intermediário responsável por tratar as probabilidades recebidas do módulo lógico e devolver uma mensagem ao usuário da aplicação;

## Executando o algoritmo

_Card-Hack_ foi construído utilizando [**_Stack_**](https://docs.haskellstack.org/en/stable/), uma ferramenta utilizada para construir projetos e gerenciar dependências em _Haskell_.

### Acessando diretório raiz do projeto

Primeiro clone o repositório de _Card-Hack-Haskell_:

```bash
git clone https://github.com/guinoronhaf/Card-Hack-Haskell.git
```

Depois acesse o diretório raiz do projeto:

```bash
cd ./Card-Hack-Haskell/card-hack-haskell/
```

### Compilando código

Utilizamos _Stack_ para compilar e executar o código. Caso não possua a ferramenta instalada em sua máquina, pode conferir o passo a passo da instalação [**aqui**](https://docs.haskellstack.org/en/stable/#how-to-install-stack).

Para compilar, executa-se:

```bash
stack build
```

**Obs.:** caso esteja compilando pela primeira vez, o processo leva mais tempo, já que todas as dependências necessárias deverão ser carregadas pela primeira vez.

### Executando código

Depois de compilado, execute o código com:

```bash
stack exec card-hack-haskell-exe
```

Um menu interativo será apresentado. Aqui, duas informações merecem destaque:

 - Consideramos que o jogador possui duas ou mais cartas, sempre;
 - Consideramos que o _Dealer_ posssui apenas uma carta (a carta que fica com a face voltada para cima no início do jogo, de acordo com as regras do _Blackjack_)

Agora, basta se divertir e ganhar todas no _Blackjack_!

## Contribuidores deste repositório

 - [Guilherme Noronha](https://github.com/guinoronhaf)
 - [João Ventura](https://github.com/joaoneto9)
 - [Lucas Cunha](https://github.com/Lucas-Cunhaa)
 - [Matteus Marinho](https://github.com/matteuscantisani)
 - [Pedro Trovão](https://github.com/PedroBMTrovao)

 ---

Repositório construído como trabalho da disciplina de Pardigmas de Linguagens de Programação, ministrado pelo Prof. Ricardo Oliveira durante o período 2025.2, no curso de Ciência da Computação da Universidade Federal de Campina Grande (UFCG).
