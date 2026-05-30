      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  25-ARGUMENTOS-ENTRADA.

       05  25-COLUNAS-DA-TABELA.
           10  25-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  25-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  25-CINDCD-TPO-EVNTO         PIC X(1).
           10  25-CINDCD-REG-LOG           PIC X(1).
           10  25-CTBELA-LOG-DESC          PIC X(8).
           10  25-CCATLG-COMPS-REG         PIC X(8).
           10  25-NSEQ-PROCM-EVNTO         PIC S9(3)V COMP-3.
           10  25-CTRANS-PROG-ATULZ        PIC X(8).
           10  25-HULT-ATULZ               PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  25-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  25-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  25-RETORNO.
       05  25-COD-RETORNO                         PIC 9(04).
       05  25-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  25-PROXIMO-RESTART                     PIC 9(05).
       05  25-QTDE-RETORNADA                      PIC 9(03).
       05  25-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  25-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T25.
           10  25RT-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  25RT-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  25RT-CINDCD-TPO-EVNTO         PIC X(1).
           10  25RT-CINDCD-REG-LOG           PIC X(1).
           10  25RT-CTBELA-LOG-DESC          PIC X(8).
           10  25RT-CCATLG-COMPS-REG         PIC X(8).
           10  25RT-NSEQ-PROCM-EVNTO         PIC S9(3)V COMP-3.
           10  25RT-CTRANS-PROG-ATULZ        PIC X(8).
           10  25RT-HULT-ATULZ               PIC X(26).
