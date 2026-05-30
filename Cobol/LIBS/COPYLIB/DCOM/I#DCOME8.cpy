      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E8 - DB2PRD.TSOLTC_RELAT_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E8-ARGUMENTOS-ENTRADA.

       05  E8-COLUNAS-DA-TABELA.
           10  E8-CSOLTC-RELAT-DESC            PIC S9(9)V COMP-3.
           10  E8-CRELAT-DESC-COML             PIC X(8).
           10  E8-DSOLTC-RELAT-DESC            PIC X(10).
           10  E8-CINDCD-DISTR-DEPDC           PIC X(1).
           10  E8-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  E8-HSIT-DESC-COML               PIC X(26).
           10  E8-HPROCM-RELAT-DESC            PIC X(26).
           10  E8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E8-HULT-ATULZ                   PIC X(26).
           10  E8-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E8-RETORNO.
       05  E8-COD-RETORNO                         PIC 9(04).
       05  E8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E8-PROXIMO-RESTART                     PIC 9(05).
       05  E8-QTDE-RETORNADA                      PIC 9(03).
       05  E8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE8.
           10  E8RT-CSOLTC-RELAT-DESC          PIC S9(9)V COMP-3.
           10  E8RT-CRELAT-DESC-COML           PIC X(8).
           10  E8RT-DSOLTC-RELAT-DESC          PIC X(10).
           10  E8RT-CINDCD-DISTR-DEPDC         PIC X(1).
           10  E8RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  E8RT-HSIT-DESC-COML             PIC X(26).
           10  E8RT-HPROCM-RELAT-DESC          PIC X(26).
           10  E8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E8RT-HULT-ATULZ                 PIC X(26).
           10  E8RT-CTERM                      PIC X(8).
