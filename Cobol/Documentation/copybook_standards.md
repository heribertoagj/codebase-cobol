# Padrão de Desenvolvimento de Copybooks - Mainframe

> [!IMPORTANT]
> Estas normas aplicam-se exclusivamente a arquivos de Copybook com extensão `.cpy`. Para programas COBOL (`.cbl`), consulte o [programa_standards.md](file:///c:/Users/herib/OneDrive/Documents/Workspace/Cobol/Documentation/programa_standards.md).

Este documento define os padrões para criação e manutenção de Copybooks (Books) no ambiente Mainframe, respeitando a arquitetura de Centros de Custo (ex: GFCT, RUCA, RUEC, Etc...).

## 1. Estrutura do Cabeçalho
Todo copybook deve iniciar com o seguinte cabeçalho padrão.
*   **Regra de Coluna**: Os asteriscos de abertura devem iniciar na **Coluna 7** (ou seja, deve haver **6 espaços** em branco antes do asterisco).

**Exemplo com Régua de Colunas:**
```text
000000000111111111122222222223
123456789012345678901234567890
      ******************************************************************
      * NOME BOOK : [NOME_DO_BOOK] - [BREVE_DESCRICAO]                 *
      * DESCRICAO : [DESCRICAO_DETALHADA_SE_NECESSARIO]                *
      * DATA      : [MM/AAAA]                                          *
      * AUTOR     : [SEU_NOME]                                         *
      * EMPRESA   : [NOME_EMPRESA]                                     *
      * TAMANHO   : [99999] BYTES                                      *
      ******************************************************************
```
Toda alteração no copybook deve incluir o seguinte trecho no cabeçalho padrão.
*   **Regra de Coluna**: Os asteriscos de abertura devem iniciar na **Coluna 7** (ou seja, deve haver **6 espaços** em branco antes do asterisco).

```text
000000000111111111122222222223
123456789012345678901234567890
[TAG]*                    A L T E R A C O E S                         *
      *----------------------------------------------------------------*
      *     DATA........: [MM/AAAA]                                    *
      *     AUTOR.......: [SEU_NOME]                                   *
      *     MOTIVO......: [DESCRICAO_DA_ALTERACAO]                     *
      ******************************************************************
```

### Regra de Marcação de Manutenção (Tag)
Ao realizar qualquer alteração no copybook, deve-se utilizar uma **Tag de Identificação** de no máximo 6 bytes nas **Colunas 1 a 6**:
*   **Formato**: `[Iniciais][AA][MM]` (ex: `HG2602`).
*   **Aplicação**: A Tag deve estar presente em todas as linhas do bloco de Alterações no cabeçalho e em todas as linhas modificadas ou novas.

## 2. Níveis de Variáveis (Level Numbers)
*   **Regra de Ouro**: Os copybooks **NÃO** devem conter o nível `01`.
*   **Padrão**: Iniciar sempre do nível **05**.
*   **Regra de Coluna**: A declaração do nível 05 deve iniciar na **Coluna 12** (Area B).

**Formato Correto:**
```cobol
0123456789012
           05 GFCTW2PJ-REGISTRO.
              10 GFCTW2PJ-BLOCO-ENTRADA.
                 15 GFCTW2PJ-E-CGC-CNPJ   PIC X(09).
```

**Formato Incorreto (EVITAR):**
```cobol
       01 GFCTW2PJ-REGISTRO.  <-- EVITAR NIVEL 01
      05 GFCTW2PJ-REGISTRO.   <-- EVITAR INICIO NA MARGEM A (Col 8-11)
```

## 3. Nomenclatura, Prefixos e Centros de Custo
Os nomes de variáveis e do arquivo devem respeitar rigorosamente o **Centro de Custo** do projeto em que se está trabalhando (ex: `GFCT`, `RUCA`, `RUEC`, `CEPT`, etc.).

*   **Nome do Arquivo**: `[CC][SISTEMA][SUFIXO].cpy`
    *   Exemplo (RUEC): `RUEC` + `W` + `9PJ` -> `ruecw9pj.cpy`
    *   Exemplo (RUCA): `RUCA` + `W` + `100` -> `rucaW100.cpy`
*   **Prefixos de Variáveis**: Devem iniciar com o identificador do Centro de Custo + Funcionalidade.
    *   Ex: Se o copybook é `RUECW9PJ`, as variáveis iniciam com `RUECW9PJ-`.

## 4. Checklist de Criação e Manutenção
1.  [ ] O nome do arquivo segue o padrão do Centro de Custo?
2.  [ ] O cabeçalho está preenchido, alinhado e possui o bloco de alterações?
3.  [ ] A tag de manutenção (ex: HG2602) foi aplicada nas colunas 1-6 das linhas alteradas?
4.  [ ] A estrutura inicia no nível 05?
5.  [ ] As variáveis possuem prefixo consistente?
