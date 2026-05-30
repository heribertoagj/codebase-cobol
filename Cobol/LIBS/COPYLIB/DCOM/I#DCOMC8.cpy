      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C8 - DB2PRD.TOPER_ROTRO_INCL       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  C8-ARGUMENTOS-ENTRADA.

       05  C8-COLUNAS-DA-TABELA.
           10  C8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C8-CTPO-AGPTO-ROTRO             PIC S9(3)V COMP-3.
           10  C8-CINDCD-OBRIG-AGPTO           PIC X(1).
           10  C8-CINDCD-SIT-AGPTO             PIC X(1).
           10  C8-CINDCD-CONCL-ROTRO           PIC X(1).
           10  C8-HULT-ATULZ                   PIC X(26).
           10  C8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C8-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C8-RETORNO.
       05  C8-COD-RETORNO                         PIC 9(04).
       05  C8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C8-PROXIMO-RESTART                     PIC 9(05).
       05  C8-QTDE-RETORNADA                      PIC 9(03).
       05  C8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC8.
           10  C8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C8RT-CTPO-AGPTO-ROTRO           PIC S9(3)V COMP-3.
           10  C8RT-CINDCD-OBRIG-AGPTO         PIC X(1).
           10  C8RT-CINDCD-SIT-AGPTO           PIC X(1).
           10  C8RT-CINDCD-CONCL-ROTRO         PIC X(1).
           10  C8RT-HULT-ATULZ                 PIC X(26).
           10  C8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C8RT-CTERM                      PIC X(8).
