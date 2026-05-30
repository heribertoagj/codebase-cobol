---
name: dclgen_interpretation
description: Skills for correctly interpreting DB2 DCLGEN copybooks in COBOL programs, mapping types and understanding structures.
---

# Interpretação de DCLGEN em COBOL

Esta skill fornece diretrizes para analisar e interpretar corretamente os copybooks gerados pelo utilitário DCLGEN do DB2 em programas COBOL.

## O que é um DCLGEN?
O DCLGEN (Declarations Generator) é uma ferramenta do DB2 que gera declarações de tabelas e estruturas de dados COBOL correspondentes. Ele garante que o programa use os tipos de dados corretos para interagir com o banco de dados.

## Estrutura do Arquivo
Um arquivo DCLGEN (geralmente com extensão `.cpy`) contém:
1. **Cabeçalho de Comentários**: Parâmetros de geração.
2. **EXEC SQL DECLARE**: Definição da tabela para o pré-compilador SQL.
3. **Estrutura COBOL (Nível 01)**: Variáveis de host para uso no código.

## Mapeamento de Tipos de Dados

Ao interpretar um DCLGEN, siga esta tabela de conversão:

| Tipo DB2 | Tipo COBOL (PIC) | Uso (USAGE) | Observações |
| :--- | :--- | :--- | :--- |
| `DECIMAL(p, s)` | `PIC S9(p-s)V9(s)` | `COMP-3` | `p` é precisão, `s` é escala. |
| `INTEGER` | `PIC S9(9)` | `COMP` / `BINARY` | Inteiro de 4 bytes. |
| `SMALLINT` | `PIC S9(4)` | `COMP` / `BINARY` | Inteiro de 2 bytes. |
| `CHAR(n)` | `PIC X(n)` | - | String de tamanho fixo. |
| `VARCHAR(n)` | Grupo Nível 49 | - | Ver seção de VARCHAR abaixo. |
| `DATE` | `PIC X(10)` | - | Formato 'AAAA-MM-DD'. |
| `TIME` | `PIC X(8)` | - | Formato 'HH.MM.SS'. |
| `TIMESTAMP` | `PIC X(26)` | - | Formato completo de data/hora. |

## Tratamento de campos VARCHAR
Campos `VARCHAR` no DB2 são gerados como um item de grupo no COBOL com dois subitens de nível 49:
- **`NOME-LEN`**: `PIC S9(4) USAGE COMP-5`. Armazena o comprimento atual do dado.
- **`NOME-TEXT`**: `PIC X(n)`. Contém o conteúdo da string.

> [!IMPORTANT]
> Ao mover dados para um campo `VARCHAR`, você deve preencher o campo `-LEN` com o tamanho da string e o campo `-TEXT` com o valor.

## Regras de Interpretação no Programa
1. **Variáveis de Host**: Sempre use a estrutura gerada no DCLGEN dentro de comandos `EXEC SQL`.
2. **Campos NULL**: Se uma coluna no `EXEC SQL DECLARE` não tiver `NOT NULL`, o DB2 pode exigir uma "Indicator Variable" (PIC S9(4) COMP) para tratar valores nulos no programa.
3. **Nomenclatura**: O DCLGEN frequentemente substitui sublinhados (`_`) por hifens (`-`) nos nomes das variáveis COBOL para conformidade com a sintaxe COBOL.

## Guia de Validação (Detecção de Erros)

Para garantir a integridade do código, sempre valide o SQL contra o DCLGEN seguindo estes passos:

1.  **Validação do Nome da Tabela**:
    - Verifique se o nome após o `FROM` ou `UPDATE` ou `INSERT INTO` corresponde exatamente ao nome declarado em `EXEC SQL DECLARE ... TABLE` no DCLGEN.
    - **Erro Comum**: Usar o nome do copybook (ex: `RUFIB002`) em vez do nome da tabela real (ex: `CONTRATO_RURAL`).

2.  **Validação de Host Variables**:
    - Certifique-se de que cada variável precedida por dois pontos (`:`) no SQL (ex: `:RUFIB002.CCONTR`) existe na estrutura COBOL do DCLGEN.
    - Verifique se a qualificação está correta (ex: `:ESTRUTURA.CAMPO`).

3.  **Mapeamento de SQLCODE**:
    - Verifique se os nomes usados em mensagens de erro (ex: `MOVE 'RUFIB002' TO ERR-DBD-TAB`) referem-se ao DCLGEN correto sendo acessado.

## Exemplo de Erro Identificado
No arquivo `ruec7951.cbl`, o seguinte trecho está incorreto:
```cobol
SELECT CCONTR FROM DB2PRD.RUFIB002
```
**Por que está errado?** De acordo com o DCLGEN `rufib002.cpy`, a tabela correta é `DB2PRD.CONTRATO_RURAL`. `RUFIB002` é apenas o nome da estrutura de dados COBOL, não o nome da tabela no DB2.
