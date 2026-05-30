      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E6 - DB2PRD.TSMULA_SELEC_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E6-ARGUMENTOS-ENTRADA.

       05  E6-COLUNAS-DA-TABELA.
           10  E6-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E6-VOPER-ARGMT-SELEC            PIC S9(15)V9(2) COMP-3.
           10  E6-VFAIXA-VLR-INIC              PIC S9(15)V9(2) COMP-3.
           10  E6-VFAIXA-VLR-FNAL              PIC S9(15)V9(2) COMP-3.
           10  E6-DFAIXA-VCTO-INIC             PIC X(10).
           10  E6-DFAIXA-VCTO-FNAL             PIC X(10).
           10  E6-QDIA-PRZ-INIC                PIC S9(3)V COMP-3.
           10  E6-QDIA-PRZ-FNAL                PIC S9(3)V COMP-3.
           10  E6-CINDCD-SELEC-VCTO            PIC S9(1)V COMP-3.
           10  E6-CINDCD-SELEC-VLR             PIC S9(1)V COMP-3.
           10  E6-CINDCD-SELEC-ESPEC           PIC X(2).
           10  E6-CINDCD-VDA-RCBVL             PIC X(1).
           10  E6-CADM-CATAO-CREDT             PIC S9(9)V COMP-3.
           10  E6-CTPO-LOTE-OPER               PIC X(1).
           10  E6-CINDCD-SELEC-CHEQ            PIC X(1).
           10  E6-CINDCD-LISTA-CART            PIC X(1).
           10  E6-CINDCD-PRZ-VCTO              PIC X(1).
           10  E6-CINDCD-SELEC-DESC            PIC X(1).
           10  E6-HULT-ATULZ                   PIC X(26).
           10  E6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E6-CTERM                        PIC X(8).
           10  E6-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E6-RETORNO.
       05  E6-COD-RETORNO                         PIC 9(04).
       05  E6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E6-PROXIMO-RESTART                     PIC 9(05).
       05  E6-QTDE-RETORNADA                      PIC 9(03).
       05  E6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE6.
           10  E6RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E6RT-VOPER-ARGMT-SELEC          PIC S9(15)V9(2) COMP-3.
           10  E6RT-VFAIXA-VLR-INIC            PIC S9(15)V9(2) COMP-3.
           10  E6RT-VFAIXA-VLR-FNAL            PIC S9(15)V9(2) COMP-3.
           10  E6RT-DFAIXA-VCTO-INIC           PIC X(10).
           10  E6RT-DFAIXA-VCTO-FNAL           PIC X(10).
           10  E6RT-QDIA-PRZ-INIC              PIC S9(3)V COMP-3.
           10  E6RT-QDIA-PRZ-FNAL              PIC S9(3)V COMP-3.
           10  E6RT-CINDCD-SELEC-VCTO          PIC S9(1)V COMP-3.
           10  E6RT-CINDCD-SELEC-VLR           PIC S9(1)V COMP-3.
           10  E6RT-CINDCD-SELEC-ESPEC         PIC X(2).
           10  E6RT-CINDCD-VDA-RCBVL           PIC X(1).
           10  E6RT-CADM-CATAO-CREDT           PIC S9(9)V COMP-3.
           10  E6RT-CTPO-LOTE-OPER             PIC X(1).
           10  E6RT-CINDCD-SELEC-CHEQ          PIC X(1).
           10  E6RT-CINDCD-LISTA-CART          PIC X(1).
           10  E6RT-CINDCD-PRZ-VCTO            PIC X(1).
           10  E6RT-CINDCD-SELEC-DESC          PIC X(1).
           10  E6RT-HULT-ATULZ                 PIC X(26).
           10  E6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E6RT-CTERM                      PIC X(8).
           10  E6RT-DINCL-REG                  PIC X(10).
