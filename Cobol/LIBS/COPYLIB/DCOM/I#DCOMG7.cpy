      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0G7 - DB2PRD.TTRASI_SELEC_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  G7-ARGUMENTOS-ENTRADA.

       05  G7-COLUNAS-DA-TABELA.
           10  G7-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  G7-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  G7-VOPER-ARGMT-SELEC            PIC S9(15)V9(2) COMP-3.
           10  G7-VFAIXA-VLR-INIC              PIC S9(15)V9(2) COMP-3.
           10  G7-VFAIXA-VLR-FNAL              PIC S9(15)V9(2) COMP-3.
           10  G7-DFAIXA-VCTO-INIC             PIC X(10).
           10  G7-DFAIXA-VCTO-FNAL             PIC X(10).
           10  G7-QDIA-PRZ-INIC                PIC S9(3)V COMP-3.
           10  G7-QDIA-PRZ-FNAL                PIC S9(3)V COMP-3.
           10  G7-CINDCD-SELEC-VCTO            PIC S9(1)V COMP-3.
           10  G7-CINDCD-SELEC-VLR             PIC S9(1)V COMP-3.
           10  G7-CINDCD-SELEC-ESPEC           PIC X(2).
           10  G7-CINDCD-VDA-RCBVL             PIC X(1).
           10  G7-CADM-CATAO-CREDT             PIC S9(9)V COMP-3.
           10  G7-CTPO-LOTE-OPER               PIC X(1).
           10  G7-CINDCD-SELEC-CHEQ            PIC X(1).
           10  G7-CINDCD-LISTA-CART            PIC X(1).
           10  G7-CINDCD-PRZ-VCTO              PIC X(1).
           10  G7-CINDCD-SELEC-DESC            PIC X(1).
           10  G7-HULT-ATULZ                   PIC X(26).
           10  G7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  G7-CTERM                        PIC X(8).
           10  G7-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  G7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  G7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  G7-RETORNO.
       05  G7-COD-RETORNO                         PIC 9(04).
       05  G7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  G7-PROXIMO-RESTART                     PIC 9(05).
       05  G7-QTDE-RETORNADA                      PIC 9(03).
       05  G7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  G7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TG7.
           10  G7RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  G7RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  G7RT-VOPER-ARGMT-SELEC          PIC S9(15)V9(2) COMP-3.
           10  G7RT-VFAIXA-VLR-INIC            PIC S9(15)V9(2) COMP-3.
           10  G7RT-VFAIXA-VLR-FNAL            PIC S9(15)V9(2) COMP-3.
           10  G7RT-DFAIXA-VCTO-INIC           PIC X(10).
           10  G7RT-DFAIXA-VCTO-FNAL           PIC X(10).
           10  G7RT-QDIA-PRZ-INIC              PIC S9(3)V COMP-3.
           10  G7RT-QDIA-PRZ-FNAL              PIC S9(3)V COMP-3.
           10  G7RT-CINDCD-SELEC-VCTO          PIC S9(1)V COMP-3.
           10  G7RT-CINDCD-SELEC-VLR           PIC S9(1)V COMP-3.
           10  G7RT-CINDCD-SELEC-ESPEC         PIC X(2).
           10  G7RT-CINDCD-VDA-RCBVL           PIC X(1).
           10  G7RT-CADM-CATAO-CREDT           PIC S9(9)V COMP-3.
           10  G7RT-CTPO-LOTE-OPER             PIC X(1).
           10  G7RT-CINDCD-SELEC-CHEQ          PIC X(1).
           10  G7RT-CINDCD-LISTA-CART          PIC X(1).
           10  G7RT-CINDCD-PRZ-VCTO            PIC X(1).
           10  G7RT-CINDCD-SELEC-DESC          PIC X(1).
           10  G7RT-HULT-ATULZ                 PIC X(26).
           10  G7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  G7RT-CTERM                      PIC X(8).
           10  G7RT-DINCL-REG                  PIC X(10).
