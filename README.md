# Trabalho Prático 1 - PFL

## Breve introdução

Este projeto consiste numa lista de funções que permitem a manipulação de polinómios. Mais concretamente, a adição, multiplicação, derivação e normalização.

## Análise da representação interna de um "polinómio"

Optamos por definir esta estrutura como uma lista de monómios, que, por sua vez, são definidos como um tuplo que contém tanto o termo independente - Double - , como uma lista referente às "variáveis" - (Char,Int) - do monímio.

- Por exemplo, o polinómio "2x^2y - 5z" transforma-se na seguinte lista: [(2,[('x',2),('y',1)]),(-5,[('z',1)])]

Escolhemos esta representação pela versatilidade que confere ao termo "polinómio", pelo que lhe permite uma gestão mais fácil e clara.

## Descrição da estratégia de implementação


### Adição de polinómios

- Inicialmente juntamos ambos os polinímios num só. 
- Em seguida são agrupados os seus monómios de acordo com as suas variáveis.
- Por fim cada uma dessas listas é transformada num monómio, formado pela soma dos termos independentes dos monómios pertencentes dessa lista, juntamente com as respetivas variáveis. 


### Multipliação de polinómios

- Realizamos a multiplicação entre cada monómio do primeiro polinómio e cada monómio do segundo.
- Essa multiplicação foi feita agrupando as variáveis de cada monómio, associando as à soma dos seus expoentes.
- Por fim cada uma dessas listas é transformada num monómio, formado pela multiplicação dos termos independentes dos monómios, juntamente com as respetivas variáveis.


### Derivação de um polinómio

- A derivação, em ordem a uma variável, de cada monómio foi feita multiplicando o expoente associado a essa variável pelo termo independente, baixando o grau dessa variável.
- Caso o monómio não tenha numa variável que corresponda a essa derivação, procedemos à anulação do mesmo.


### Normalização de um polinómio

- Procedemos à ordenação de acordo com o maior grau da menor variável (alfabeticamente).
- Retiramos os monómios e variáveis nulas
- Somamos os diferentes monómios associados de forma a garantir que o resultado está simplificado.


## Como utilizar

- Tenha o ghci instalado no seu computador.   https://www.haskell.org/ghc/
- Corra o comando "ghci" no terminal, estando no diretorio neste projeto.
- Execute o seguinte comando: ":load main.hs"


## Exemplos de utilização

### Adicionar

- Comando a utilizar: addPolynomials <polinómio 1> <polinómio 2>

- addPolynomials "x + 3y + x^2 - 2" "3 - x + y^2 - y" <br>
"aa"

- addPolynomials "2x^2y - 1x^2 + 5y - z" "3x^2 + 2 - 2y" <br>
"aaa"

### Multiplicar

- Comando a utilizar: multiplyPolynomials <polinómio 1> <polinómio 2>

- multiplyPolynomials "x + x^2 + 3yz" "y + x^2z + 1" <br>
"x^4+x^3+4x^2+4y+4x^2+4z+x+y+x+z+3y^2+6y+6z+3z^2"

- multiplyPolynomials "x + 2y + xy + 1" "y + x^2 + 3" <br>
"aaa"


### Derivar

- Comando a utilizar: derivatePolynomial <polinómio> <variável>

- derivatePolynomial "x + 3y + x^2 - 2" 'x' <br>
"2x + 1"

- derivatePolynomial "2x^2y^3 - 1x^2 + 5y^2 + 2x - 3z + 1 + y + 2y" 'y' <br>
"6x^2y^2 + 10y + 3"

### Normalizar

- Comando a utilizar: normalizePolynomial <polinómio>))

- normalizePolynomial "2y + 4xz + 2x^2 - 8z + 1 + 2y" <br>
""

- normalizePolynomial "0x^2 - 3y + 3x^0y + y + 2x + x^2 - 1" <br>
""
