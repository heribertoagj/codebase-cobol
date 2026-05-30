      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  72-ARGUMENTOS-ENTRADA.

       05  72-COLUNAS-DA-TABELA.
           10  72-CPRODT                   PIC S9(3)V COMP-3.
           10  72-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  72-HINIC-TX-SPROD           PIC X(26).
           10  72-CTPO-TX-JURO-DESC        PIC S9(3)V COMP-3.
           10  72-HFIM-TX-SPROD            PIC X(26).
           10  72-HULT-ATULZ               PIC X(26).
           10  72-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  72-CTERM                    PIC X(8).
           10  72-QDIA-OPER-ATIVO          PIC S9(4)V COMP-3.
           10  72-CINDCD-FREQ-ESTOQ        PIC X(2).
           10  72-CINDCD-OPER-ATRSO        PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  72-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  72-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  72-RETORNO.
       05  72-COD-RETORNO                         PIC 9(04).
       05  72-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  72-PROXIMO-RESTART                     PIC 9(05).
       05  72-QTDE-RETORNADA                      PIC 9(03).
       05  72-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  72-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T72.
           10  72RT-CPRODT                 PIC S9(3)V COMP-3.
           10  72RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  72RT-HINIC-TX-SPROD         PIC X(26).
           10  72RT-CTPO-TX-JURO-DESC      PIC S9(3)V COMP-3.
           10  72RT-HFIM-TX-SPROD          PIC X(26).
           10  72RT-HULT-ATULZ             PIC X(26).
           10  72RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  72RT-CTERM                  PIC X(8).
           10  72RT-QDIA-OPER-ATIVO        PIC S9(4)V COMP-3.
           10  72RT-CINDCD-FREQ-ESTOQ      PIC X(2).
           10  72RT-CINDCD-OPER-ATRSO      PIC X(1).
