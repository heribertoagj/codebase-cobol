      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E7 - DB2PRD.TSMULA_TITLO_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************

       01  E7-ARGUMENTOS-ENTRADA.

       05  E7-COLUNAS-DA-TABELA.
           10  E7-NSMULA-OPER-DESC             PIC S9(9)V      COMP-3.
           10  E7-NTITLO-DESC-COML             PIC S9(5)V      COMP-3.
           10  E7-CBCO-PRODT-COBR              PIC S9(3)V      COMP-3.
           10  E7-CIDTFD-PRODT-COBR            PIC S9(2)V      COMP-3.
           10  E7-CNEGOC-COBR                  PIC S9(18)V     COMP-3.
           10  E7-CTITLO-COBR-BCO              PIC S9(18)V     COMP-3.
           10  E7-CNRO-SEQ-TITLO               PIC S9(3)V      COMP-3.
           10  E7-DEMIS-TITLO                  PIC X(10).
           10  E7-DVCTO-TITLO-DESC             PIC X(10).
           10  E7-VTITLO-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  E7-CINDCD-SELEC-DESC            PIC X(1).
           10  E7-HULT-ATULZ                   PIC X(26).
           10  E7-CFUNC-BDSCO                  PIC S9(9)V      COMP-3.
           10  E7-CTERM                        PIC X(8).
           10  E7-DINCL-REG                    PIC X(10).
           10  E7-CADM-CATAO-CREDT             PIC S9(9)V      COMP-3.
           10  E7-CINDCD-VDA-RCBVL             PIC X(01).
           10  E7-VORPAG-RCBVL                 PIC S9(15)V9(2) COMP-3.
           10  E7-IPSSOA-SACDO                 PIC X(60).
           10  E7-CCNPJ-CPF                    PIC S9(9)       COMP-3.
           10  E7-CFLIAL-CNPJ                  PIC S9(5)       COMP-3.
           10  E7-CCTRL-CNPJ-CPF               PIC S9(2)       COMP-3.
           10  E7-CSEU-NRO-TITLO               PIC X(15).
           10  E7-CESPCE-TITLO-COBR            PIC S9(3)       COMP-3.
           10  E7-CIDTFD-PRODT-ORIGN           PIC S9(2)       COMP-3.
DTS001     10  E7-NNOTA-PROMS-RURAL            PIC S9(11)V     COMP-3.
DTS001     10  E7-CINDCD-NOTA-PROMS            PIC  X(03).
DTS001     10  E7-CTPO-PROTR-RURAL             PIC S9(02)V     COMP-3.
DTS001     10  E7-CPRODT-RURAL                 PIC S9(03)V     COMP-3.
DTS001     10  E7-QPRODT-RURAL                 PIC S9(09)V     COMP-3.
DTS001     10  E7-CUND-MEDD                    PIC S9(03)V     COMP-3.
DTS001     10  E7-CINDCD-PGDOR-JURO            PIC  X(01).
DTS001     10  E7-CINDCD-INSTR-DESC            PIC S9(01)V     COMP-3.
DTS001     10  E7-CCEP                         PIC S9(05)V     COMP-3.
DTS001     10  E7-CCEP-COMPL                   PIC S9(03)V     COMP-3.
DTS001     10  E7-ELOGDR                       PIC  X(30).
DTS001     10  E7-IBAIRO                       PIC  X(40).
DTS001     10  E7-IMUN                         PIC  X(30).
DTS001     10  E7-CSGL-UF                      PIC  X(02).
DTS001     10  E7-VABTMT-TITLO-DESC            PIC S9(15)V9(02) COMP-3.
DTS001     10  E7-CORIGE-REC                   PIC S9(03)V     COMP-3.
DTS001     10  E7-CBCO-DEPOS                   PIC S9(03)V     COMP-3.
DTS001     10  E7-CAG-BCRIA-DEPOS              PIC S9(05)V     COMP-3.
DTS001     10  E7-RANLSE-CREDT-CLI             PIC  X(20).
DTS001     10  E7-DSOLTC-ANLSE-CREDT           PIC  X(10).
DTS001     10  E7-CSIT-ANLSE-CREDT             PIC S9(03)V     COMP-3.
DTS001     10  E7-CINDCD-ISENC-ANLSE           PIC  X(01).
DTS001     10  E7-CINDCD-PROTE-TITLO           PIC  X(01).
DTS001     10  E7-QDIA-PROTE-TITLO             PIC S9(03)V     COMP-3.
DTS001     10  E7-CBCO-DEB-AUTOM               PIC S9(03)V     COMP-3.
DTS001     10  E7-CAG-DEB-AUTOM                PIC S9(05)V     COMP-3.
DTS001     10  E7-CCTA-DEB-AUTOM               PIC S9(13)V     COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E7-RETORNO.
       05  E7-COD-RETORNO                         PIC 9(04).
       05  E7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E7-PROXIMO-RESTART                     PIC 9(05).
       05  E7-QTDE-RETORNADA                      PIC 9(03).
       05  E7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE7.
           10  E7RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E7RT-NTITLO-DESC-COML           PIC S9(5)V COMP-3.
           10  E7RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  E7RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  E7RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  E7RT-CTITLO-COBR-BCO            PIC S9(18)V COMP-3.
           10  E7RT-CNRO-SEQ-TITLO             PIC S9(3)V COMP-3.
           10  E7RT-DEMIS-TITLO                PIC X(10).
           10  E7RT-DVCTO-TITLO-DESC           PIC X(10).
           10  E7RT-VTITLO-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  E7RT-CINDCD-SELEC-DESC          PIC X(1).
           10  E7RT-HULT-ATULZ                 PIC X(26).
           10  E7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E7RT-CTERM                      PIC X(8).
           10  E7RT-DINCL-REG                  PIC X(10).
           10  E7RT-CADM-CATAO-CREDT           PIC S9(9)V COMP-3.
           10  E7RT-CINDCD-VDA-RCBVL           PIC X(01).
           10  E7RT-VORPAG-RCBVL               PIC S9(15)V9(2) COMP-3.
           10  E7RT-IPSSOA-SACDO               PIC X(60).
           10  E7RT-CCNPJ-CPF                  PIC S9(9) COMP-3.
           10  E7RT-CFLIAL-CNPJ                PIC S9(5) COMP-3.
           10  E7RT-CCTRL-CNPJ-CPF             PIC S9(2) COMP-3.
           10  E7RT-CSEU-NRO-TITLO             PIC X(15).
           10  E7RT-CESPCE-TITLO-COBR          PIC S9(3) COMP-3.
           10  E7RT-CIDTFD-PRODT-ORIGN         PIC S9(2) COMP-3.
DTS001     10  E7RT-NNOTA-PROMS-RURAL          PIC S9(11)V     COMP-3.
DTS001     10  E7RT-CINDCD-NOTA-PROMS          PIC  X(03).
DTS001     10  E7RT-CTPO-PROTR-RURAL           PIC S9(02)V     COMP-3.
DTS001     10  E7RT-CPRODT-RURAL               PIC S9(03)V     COMP-3.
DTS001     10  E7RT-QPRODT-RURAL               PIC S9(09)V     COMP-3.
DTS001     10  E7RT-CUND-MEDD                  PIC S9(03)V     COMP-3.
DTS001     10  E7RT-CINDCD-PGDOR-JURO          PIC  X(01).
DTS001     10  E7RT-CINDCD-INSTR-DESC          PIC S9(01)V     COMP-3.
DTS001     10  E7RT-CCEP                       PIC S9(05)V     COMP-3.
DTS001     10  E7RT-CCEP-COMPL                 PIC S9(03)V     COMP-3.
DTS001     10  E7RT-ELOGDR                     PIC  X(30).
DTS001     10  E7RT-IBAIRO                     PIC  X(40).
DTS001     10  E7RT-IMUN                       PIC  X(30).
DTS001     10  E7RT-CSGL-UF                    PIC  X(02).
DTS001     10  E7RT-VABTMT-TITLO-DESC          PIC S9(15)V9(02) COMP-3.
DTS001     10  E7RT-CORIGE-REC                 PIC S9(03)V     COMP-3.
DTS001     10  E7RT-CBCO-DEPOS                 PIC S9(03)V     COMP-3.
DTS001     10  E7RT-CAG-BCRIA-DEPOS            PIC S9(05)V     COMP-3.
DTS001     10  E7RT-RANLSE-CREDT-CLI           PIC  X(20).
DTS001     10  E7RT-DSOLTC-ANLSE-CREDT         PIC  X(10).
DTS001     10  E7RT-CSIT-ANLSE-CREDT           PIC S9(03)V     COMP-3.
DTS001     10  E7RT-CINDCD-ISENC-ANLSE         PIC  X(01).
DTS001     10  E7RT-CINDCD-PROTE-TITLO         PIC  X(01).
DTS001     10  E7RT-QDIA-PROTE-TITLO           PIC S9(03)V     COMP-3.
DTS001     10  E7RT-CBCO-DEB-AUTOM             PIC S9(03)V     COMP-3.
DTS001     10  E7RT-CAG-DEB-AUTOM              PIC S9(05)V     COMP-3.
DTS001     10  E7RT-CCTA-DEB-AUTOM             PIC S9(13)V     COMP-3.
