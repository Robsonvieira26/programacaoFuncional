# Respostas

## Questão 2

| Variaçao1 Interações | Variaçao2 Interações | Variaçao3 Interações | Lista |
| -------------------- | -------------------- | -------------------- | ----- |
| 999                  | 499500               | 999                  | l1    |
| 999000               | 499500               | 499500               | l2    |
| 1001000              | 500500               | 500500               | l3    |
| 1000000              | 500500               | 500499               | l4    |
| 3998000              | 2001000              | 2000997              | l5    |
| 2002000              | 2001000              | 1500500              | l6    |
| 3998000              | 2001000              | 2000997              | l7    |
| 19                   | 190                  | 19                   | x1    |
| 380                  | 190                  | 190                  | x2    |
| 209                  | 190                  | 145                  | x3    |
| 190                  | 190                  | 135                  | x4    |
| 285                  | 190                  | 175                  | x5    |
| 247                  | 190                  | 162                  | x6    |
| 285                  | 190                  | 175                  | x7    |

Aparentemente as variações do Algoritmo Bubble executam em tempos similares, porem a quantidade de interações que a variação 3 realiza em relação a variação 1 é enorme, mas em comparação a variação 2 quando possui diferença na quantidade de interações é mínima (considerando tempo de execução). Desta forma elegemos a Variação 3 como a melhor em relação as outras.

## Questão 3

| Variaçao2 Interações | Lista |
| -------------------- | ----- |
| 499500               | l1    |
| 499500               | l2    |
| 500500               | l3    |
| 500500               | l4    |
| 2001000              | l5    |
| 2001000              | l6    |
| 2001000              | l7    |
| 190                  | x1    |
| 190                  | x2    |
| 190                  | x3    |
| 190                  | x4    |
| 190                  | x5    |
| 190                  | x6    |
| 190                  | x7    |

Acreditamos que a necessidade da variação 2 tenha um desempenho melhor principalmente por causa da sua implementação. A necessidade de achar o menor elemento e remove-ló da lista sem percorrer duas vezes utiliza mais processamento por manter o valor fora da lista e fazer a sua inserção de volta quando um valor menor que ele for encontrado. Desta forma elegemos a Variação 1 como a melhor em relação a outra.

## Questão 4

| Variaçao1 Interações | Variaçao2 Interações | Listas |
| -------------------- | -------------------- | ------ |
| 499500               | 503500               | l1     |
| 499500               | 172165               | l2     |
| 499501               | 503505               | l3     |
| 500500               | 173169               | l4     |
| 1001000              | 1009007              | l5     |
| 1001000              | 347668               | l6     |
| 1001000              | 346336               | l7     |
| 190                  | 270                  | x1     |
| 190                  | 175                  | x2     |
| 100                  | 180                  | x3     |
| 100                  | 170                  | x4     |
| 80                   | 170                  | x5     |
| 84                   | 196                  | x6     |
| 81                   | 180                  | x7     |

Em questões de interações a variação 2 é superior pois em media realiza menos testes que sua concorrente. Entretanto devido a maior divergência das duas serem na escolha do pivô esse é o ponto que causa tal divergência. Dependendo da complexidade da lista a ser ordenada a Variação 1 tem vantagem. Considerando os testes a cima elegemos a Variação 2 como a melhor em relação a outra.

## Questão 5

| Bubble - V3 | Selection - V2 | QuickSort - V2 | Listas |
| ----------- | -------------- | -------------- | ------ |
| 999         | 499500         | 503500         | l1     |
| 499500      | 499500         | 172165         | l2     |
| 500500      | 500500         | 503505         | l3     |
| 500499      | 500500         | 173169         | l4     |
| 2000997     | 2001000        | 1009007        | l5     |
| 1500500     | 2001000        | 347668         | l6     |
| 2000997     | 2001000        | 346336         | l7     |
| 19          | 190            | 270            | x1     |
| 190         | 190            | 175            | x2     |
| 145         | 190            | 180            | x3     |
| 135         | 190            | 170            | x4     |
| 175         | 190            | 170            | x5     |
| 162         | 190            | 196            | x6     |
| 175         | 190            | 180            | x7     |

Observando as comparações e a velocidade aparente de execução nos algoritmos o QuickSort mesmo comparando mais vezes que alguns dos outros métodos ainda possui um tempo de resposta aparentemente maior.
