      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F9 - DB2PRD.TTRASI_CTRL_DIGTC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F9-ARGUMENTOS-ENTRADA.

       05  F9-COLUNAS-DA-TABELA.
           10  F9-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F9-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F9-QTITLO-DIGTC-AG              PIC S9(3)V COMP-3.
           10  F9-VTOT-TITLO-DIGTC             PIC S9(15)V9(2) COMP-3.
           10  F9-CINDCD-CONCL-DIGTC           PIC X(1).
           10  F9-HULT-ATULZ                   PIC X(26).
           10  F9-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F9-RETORNO.
       05  F9-COD-RETORNO                         PIC 9(04).
       05  F9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F9-PROXIMO-RESTART                     PIC 9(05).
       05  F9-QTDE-RETORNADA                      PIC 9(03).
       05  F9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF9.
           10  F9RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F9RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F9RT-QTITLO-DIGTC-AG            PIC S9(3)V COMP-3.
           10  F9RT-VTOT-TITLO-DIGTC           PIC S9(15)V9(2) COMP-3.
           10  F9RT-CINDCD-CONCL-DIGTC         PIC X(1).
           10  F9RT-HULT-ATULZ                 PIC X(26).
           10  F9RT-CTERM                      PIC X(8).
