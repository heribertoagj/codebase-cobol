      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H7 - DB2PRD.TTRASI_CTA_CHEQC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  H7-ARGUMENTOS-ENTRADA.

       05  H7-COLUNAS-DA-TABELA.
           10  H7-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  H7-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  H7-CBCO-CLI-CHEQC               PIC S9(3)V COMP-3.
           10  H7-CDEPDC                       PIC S9(5)V COMP-3.
           10  H7-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  H7-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  H7-CINDCD-SELEC-DESC            PIC X(1).
           10  H7-HULT-ATULZ                   PIC X(26).
           10  H7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H7-CTERM                        PIC X(8).
           10  H7-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H7-RETORNO.
       05  H7-COD-RETORNO                         PIC 9(04).
       05  H7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H7-PROXIMO-RESTART                     PIC 9(05).
       05  H7-QTDE-RETORNADA                      PIC 9(03).
       05  H7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH7.
           10  H7RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  H7RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  H7RT-CBCO-CLI-CHEQC             PIC S9(3)V COMP-3.
           10  H7RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  H7RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  H7RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  H7RT-CINDCD-SELEC-DESC          PIC X(1).
           10  H7RT-HULT-ATULZ                 PIC X(26).
           10  H7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H7RT-CTERM                      PIC X(8).
           10  H7RT-DINCL-REG                  PIC X(10).
