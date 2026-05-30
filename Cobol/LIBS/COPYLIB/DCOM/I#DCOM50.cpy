      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  50-ARGUMENTOS-ENTRADA.

       05  50-COLUNAS-DA-TABELA.
           10  50-CPRODT                   PIC S9(3)V COMP-3.
           10  50-CREGRA-IOF-DESC          PIC S9(3)V COMP-3.
           10  50-DINIC-REGRA-IOF          PIC X(10).
           10  50-IREGRA-IOF-DESC          PIC X(40).
           10  50-CINDCD-PSSOA-TOMAD       PIC X(1).
  LT       10  50-CFORMA-CNSTT-PJ          PIC S9(2)V COMP-3.
  LT  *    10  50-CNATUZ-JURID             PIC S9(3)V COMP-3.
           10  50-CTPO-RAMO-ATVDD          PIC S9(3)V COMP-3.
           10  50-CPORTE-EMPR              PIC S9(3)V COMP-3.
           10  50-CINDCD-LOGCO-VLR         PIC X(2).
           10  50-VREFT-OPER-DESC          PIC S9(15)V9(2) COMP-3.
           10  50-CINDCD-LOGCO-PRZ         PIC X(2).
           10  50-TREFT-OPER-DESC          PIC S9(3)V COMP-3.
           10  50-PALIQT-IOF-DESC          PIC S9(3)V9(5) COMP-3.
           10  50-CINDCD-FREQ-ALIQT        PIC X(2).
           10  50-DFIM-REGRA-IOF           PIC X(10).
           10  50-HULT-ATULZ               PIC X(26).
           10  50-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  50-CTERM                    PIC X(8).
           10  50-CINDCD-ALIQT-REDZD       PIC X(1).
           10  50-PALIQT-IOF-REDZD         PIC S9(3)V9(5) COMP-3.
           10  50-CFREQ-ALIQT-REDZD        PIC X(2).
           10  50-DSIT-REGRA-IOF           PIC X(10).
           10  50-CSIT-REGRA-IOF           PIC S9(1)V COMP-3.
           10  50-CTPO-ACAO-REGRA          PIC X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  50-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  50-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  50-RETORNO.
       05  50-COD-RETORNO                         PIC 9(04).
       05  50-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  50-PROXIMO-RESTART                     PIC 9(05).
       05  50-QTDE-RETORNADA                      PIC 9(03).
       05  50-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  50-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T50.
           10  50RT-CPRODT                 PIC S9(3)V COMP-3.
           10  50RT-CREGRA-IOF-DESC        PIC S9(3)V COMP-3.
           10  50RT-DINIC-REGRA-IOF        PIC X(10).
           10  50RT-IREGRA-IOF-DESC        PIC X(40).
           10  50RT-CINDCD-PSSOA-TOMAD     PIC X(1).
  LT       10  50RT-CFORMA-CNSTT-PJ        PIC S9(2)V COMP-3.
  LT  *    10  50RT-CNATUZ-JURID           PIC S9(3)V COMP-3.
           10  50RT-CTPO-RAMO-ATVDD        PIC S9(3)V COMP-3.
           10  50RT-CPORTE-EMPR            PIC S9(3)V COMP-3.
           10  50RT-CINDCD-LOGCO-VLR       PIC X(2).
           10  50RT-VREFT-OPER-DESC        PIC S9(15)V9(2) COMP-3.
           10  50RT-CINDCD-LOGCO-PRZ       PIC X(2).
           10  50RT-TREFT-OPER-DESC        PIC S9(3)V COMP-3.
           10  50RT-PALIQT-IOF-DESC        PIC S9(3)V9(5) COMP-3.
           10  50RT-CINDCD-FREQ-ALIQT      PIC X(2).
           10  50RT-DFIM-REGRA-IOF         PIC X(10).
           10  50RT-HULT-ATULZ             PIC X(26).
           10  50RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  50RT-CTERM                  PIC X(8).
           10  50RT-CINDCD-ALIQT-REDZD     PIC X(1).
           10  50RT-PALIQT-IOF-REDZD       PIC S9(3)V9(5) COMP-3.
           10  50RT-CFREQ-ALIQT-REDZD      PIC X(2).
           10  50RT-DSIT-REGRA-IOF         PIC X(10).
           10  50RT-CSIT-REGRA-IOF         PIC S9(1)V COMP-3.
           10  50RT-CTPO-ACAO-REGRA        PIC X(1).
