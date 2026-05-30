# Padrão de Desenvolvimento de Programas COBOL - Mainframe

> [!IMPORTANT]
> Estas normas aplicam-se exclusivamente a arquivos de programa com extensão `.cbl`. Para Copybooks (`.cpy`), consulte o [copybook_standards.md](file:///c:/Users/herib/OneDrive/Documents/Workspace/Cobol/Documentation/copybook_standards.md).

Este documento define os padrões de codificação e alinhamento para programas COBOL, garantindo legibilidade e manutenção simplificada.

## 1. Regras de Colunas (Layout Fixo)

O COBOL segue um layout rígido de colunas que deve ser respeitado rigorosamente:

*   **Colunas 1-6**: Reservadas para Sequence Numbers e **Marcação de Manutenção** (Tag de identificação).
*   **Coluna 7**: Margem de Indicadores (Asterisco `*` para comentários, Traço `-` para continuação de literais).
*   **Colunas 8-11 (Margem A)**: Início de `DIVISION`, `SECTION`, Nomes de Parágrafos e nível `01`.
*   **Colunas 12-72 (Margem B)**: Comandos COBOL, níveis de dados (03, 05, etc.) e complementos.

### Alinhamento Especial em Coluna 40
Para máxima organização e padronização, os seguintes elementos devem iniciar obrigatoriamente na **Coluna 40**:
*   **Complements de Divisões/Seções**: ex: `IDENTIFICATION             DIVISION.` ou `WORKING-STORAGE           SECTION.`
*   **Cláusulas de Variáveis**: `PIC`, `USAGE`, `VALUE` devem iniciar exatamente na **Coluna 40**.
    *   **Níveis de Dados**: O nível `01` fica na coluna 8. Níveis subsequentes (`03`, `05`, `07`, etc.) devem ser indentados em **3 espaços** cada.

**Exemplo de Declaração:**
```cobol
       01 WRK-AREA-DADOS.
          03 WRK-CABECALHO.
             05 WRK-DATA              PIC X(010)    VALUE SPACES.
             05 WRK-HORA              PIC X(008)    VALUE SPACES.
          03 WRK-CONTEUDO             PIC X(100)    VALUE SPACES.
```

*   **Complements de Comandos**: O `TO` do `MOVE`, o `INTO` do `EXEC SQL`, o `USING` do `CALL`, etc.
    *   **Múltiplos alvos**: Quando um comando tem vários alvos, todos devem alinhar na Coluna 40.
    *   **Cláusula OF**: Também deve ser alinhada na Coluna 40 quando possível para clareza.
*   **Comando EXIT**: Deve ficar na **mesma linha** do parágrafo `-99-FIM.`, iniciando na Coluna 40. (ex: `3000-99-FIM.                    EXIT.`)

**Exemplo de Comando MOVE:**
```cobol
           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC OF RUECV007.
           MOVE WRK-241-X              TO WRK-O-ITEMLS-ATTR(IND-1)
                                          WRK-O-CHVOPE-ATTR(IND-1)
                                          WRK-O-SLDPRI-ATTR(IND-1).
```


## 2. Cabeçalho Padrão e Histórico

Todo programa deve conter o bloco de identificação e o histórico de alterações alinhados. 

### Regra de Marcação de Manutenção (Tag)
Ao realizar qualquer alteração, deve-se utilizar uma **Tag de Identificação** de no máximo 6 bytes nas **Colunas 1 a 6**:
*   **Formato**: `[Iniciais][AA][MM]` (ex: `HG2602` para Heriberto Giannasi em Fevereiro/2026).
*   **Aplicação**: A Tag deve estar presente em:
    *   Todas as linhas do bloco de Alterações no cabeçalho.
    *   Todas as linhas de código modificadas ou novas.

```cobol
      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.                     [NOME_PGM].
       AUTHOR.                         [NOM_AUTOR].
      *================================================================*
      *                         [EMPRESA]                              *
      *----------------------------------------------------------------*
      *     PROGRAMA....: [NOME_PGM]                                   *
      *     DATA........: [MM/AAAA]                                    *
      *     OBJETIVO....: [DESCRICAO_CURTA]                            *
      *----------------------------------------------------------------*
      *     COPYBOOKS UTILIZADOS :                                     *
      *     - [NOME_BOOK]                                              *
      *----------------------------------------------------------------*
      *     MODULOS :                                                  *
      *     - [NOME_MODULO]                                            *
      *----------------------------------------------------------------*
      *     TABELAS :                                                  *
      *     - [NOME_TABELA]                                            *
      *----------------------------------------------------------------*
```

Toda alteração no programa deve incluir o seguinte trecho no cabeçalho padrão.
*   **Regra de Coluna**: Os asteriscos de abertura devem iniciar na **Coluna 7** (ou seja, deve haver **6 espaços** em branco antes do asterisco).

```text
000000000111111111122222222223
123456789012345678901234567890
[TAG]*                    A L T E R A C O E S                         *
      *----------------------------------------------------------------*
      *     DATA........: [DATA DA ALTERAÇÃO]                          *
      *     AUTOR.......: [NOM_AUTOR]                                  *
      *     MOTIVO......: [DESCRICAO DA ALTERACAO]                     *
      *================================================================*

```


> [!TIP]
> O valor de `[EMPRESA]` deve ser solicitado ao usuarioa solicitante.
> O valor de `[CENTRO_CUSTO]` deve compor o nome do arquivo e do programa, ele tambem deve ser solicitado ao usuarioa solicitante.

## 3. Estrutura da Procedure Division

### Uso de Sections e Parágrafos
*   Utilize `SECTION` para agrupar funcionalidades lógicas.
*   Todo `PERFORM` deve preferencialmente chamar uma `SECTION`.
*   Cada `SECTION` deve terminar com um parágrafo de saída nomeado `[NOME-SECTION]-99-FIM.` com o comando `EXIT` na mesma linha (Col 40).

**Exemplo:**
```cobol
    3000-PROCESSAR                  SECTION.
   *----------------------------------------------------------------*
        PERFORM 3100-LER-TABELA.
        ...
    3000-99-FIM.                    EXIT.
```

### 3.1. Identação de Blocos de Controle
Todo bloco lógico deve ter seu conteúdo indentado por **3 a 4 espaços** em relação ao comando de abertura.

*   **IF/ELSE**:
```cobol
           IF VAR-A EQUAL VAR-B
              MOVE SPACES              TO VAR-C
           ELSE
              MOVE 'ERRO'              TO VAR-C
           END-IF.
```

*   **PERFORM VARYING**:
```cobol
           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1 GREATER 8
              MOVE SPACES              TO TABELA(IND-1)
           END-PERFORM.
```

*   **EVALUATE**: O `WHEN` alinha com o `EVALUATE`.
```cobol
           EVALUATE TRUE
           WHEN WRK-PFK EQUAL 'C'
              PERFORM 3300-TRATAR-PF3
           WHEN OTHER
              CONTINUE
           END-EVALUATE.
```

### 3.2. Formatação de EXEC SQL
Comandos SQL embutidos devem seguir a margem B (col 12) e identar seus componentes internos.

```cobol
           EXEC SQL
             UPDATE DB2PRD.NAVEGACAO_RURAL
             SET WNAVG_CREDT_RURAL = :VAR-SQL
             WHERE CTERM = :VAR-TERM
           END-EXEC.
```


## 4. Melhores Práticas de Codificação

*   **EVALUATE vs IF**: Prefira `EVALUATE TRUE` para múltiplas condições em vez de `IF` aninhados.
*   **Tipos de Dados**:
    *   `COMP` (ou `BINARY`) para índices e contadores.
    *   `COMP-3` para cálculos e campos financeiros.
*   **Nomenclatura**: Use prefixos claros (ex: `WRK-` para Working-Storage, `LNK-` para Linkage).

## 5. Checklist de Qualidade
1. [ ] As Divisões e Seções estão alinhadas na Coluna 8 com complemento na Coluna 40?
2. [ ] A cláusula `PIC` está alinhada na Coluna 40?
3. [ ] Os parágrafos de `EXIT` estão alinhados na Coluna 40?
4. [ ] O cabeçalho está completo e padronizado?
5. [ ] Foi utilizado `EVALUATE` em vez de `IF` complexos?
