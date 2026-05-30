      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I4 - DB2PRD.THIST_PERAC_FORML      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I4-ARGUMENTOS-ENTRADA.

       05  I4-COLUNAS-DA-TABELA.
           10  I4-CELMTO-DESC-COML            PIC S9(003)V COMP-3.
           10  I4-HPAREC-FORML-DESC           PIC  X(026).
           10  I4-CCHAVE-ELMTO-DESC           PIC  X(045).
           10  I4-RPRCER-FORML-DESC.
               49  I4-RPRCER-FORML-DESC-LEN   PIC S9(004)  COMP.
               49  I4-RPRCER-FORML-DESC-TEXT  PIC  X(240).
           10  I4-HULT-ATULZ                  PIC  X(026).
           10  I4-CFUNC-BDSCO                 PIC S9(009)V COMP-3.
           10  I4-CTERM                       PIC  X(008).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I4-QTDE-A-DESPREZAR                PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I4-INSTRUCAO                       PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I4-RETORNO.
           05  I4-COD-RETORNO                 PIC  9(004).
           05  I4-MENSAGEM                    PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
           05  I4-PROXIMO-RESTART             PIC  9(005).
           05  I4-QTDE-RETORNADA              PIC  9(003).
           05  I4-QTDE-TOTAL                  PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
           05  I4-OCORRENCIAS  OCCURS 070  TIMES INDEXED BY IND-TI4.
               10  I4RT-CELMTO-DESC-COML          PIC S9(003)V COMP-3.
               10  I4RT-HPAREC-FORML-DESC         PIC  X(026).
               10  I4RT-CCHAVE-ELMTO-DESC         PIC  X(045).
               10  I4RT-RPRCER-FORML-DESC.
                   49 I4RT-RPRCER-FORML-DESC-LEN  PIC S9(004)  COMP.
                   49 I4RT-RPRCER-FORML-DESC-TEXT PIC  X(240).
               10  I4RT-HULT-ATULZ                PIC  X(026).
               10  I4RT-CFUNC-BDSCO               PIC S9(009)V COMP-3.
               10  I4RT-CTERM                     PIC  X(008).
