      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB095 - DB2PRD.TCHEQC_MODIF_PCELA     *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
BSI001*                    ALTERACAO - BSI TECNOLOGIA                 *
BSI001*---------------------------------------------------------------*
BSI001*  ANALISTA BSI:   FERNANDO LUIZ DE SANTI  -  BSI TECNOLOGIA    *
BSI001*  DATA........:   28/04/2017                                   *
BSI001*---------------------------------------------------------------*
BSI001*  OBJETIVO....:   INCLUIR CAMPOS DE ENCARGOS MORATORIOS.       *
BSI001*****************************************************************
       01  95-ARGUMENTOS-ENTRADA.

       05  95-COLUNAS-DA-TABELA.
           10  95-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  95-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  95-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  95-CBCO                         PIC S9(3)V COMP-3.
           10  95-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  95-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  95-CCHEQ                        PIC S9(7)V COMP-3.
           10  95-HULT-ATULZ                   PIC X(26).
           10  95-CDEPDC                       PIC S9(5)V COMP-3.
           10  95-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  95-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  95-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  95-VCHEQC                       PIC S9(13)V9(2) COMP-3.
           10  95-DMODIF-CHEQC-PCELA           PIC X(10).
           10  95-RMOTVO-DEVLC-CHEQC.
              49  95-RMOTVO-DEVLC-CHEQC-LEN    PIC S9(4) COMP.
              49  95-RMOTVO-DEVLC-CHEQC-TEXT   PIC X(250).
           10  95-CINDCD-REAP-CHEQC            PIC S9(1)V COMP-3.
           10  95-DREAP-CHEQC                  PIC X(10).
           10  95-CINDCD-MODLD-PROCM           PIC X(1).
           10  95-CINDCD-MOTVO-MODIF           PIC X(1).
           10  95-VSDO-DVDOR-PRINC             PIC S9(15)V9(2) COMP-3.
           10  95-VJURO-MORA-LEGAL             PIC S9(15)V9(2) COMP-3.
           10  95-VJURO-MORA-GERC              PIC S9(15)V9(2) COMP-3.
           10  95-VIOF-PCELA-VENCD             PIC S9(15)V9(2) COMP-3.
           10  95-VCUSTO-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  95-VJURO-TRNSF-CRATS            PIC S9(15)V9(2) COMP-3.
           10  95-VCUSTO-TRNSF-CRATS           PIC S9(15)V9(2) COMP-3.
           10  95-CCONTR-CORP-CHEQC            PIC S9(7)V COMP-3.
           10  95-DVNCLO-REAP-CHEQC            PIC X(10).
BSI001     10  95-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  95-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  95-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  95-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  95-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  95-RETORNO.
       05  95-COD-RETORNO                         PIC 9(04).
       05  95-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  95-PROXIMO-RESTART                     PIC 9(05).
       05  95-QTDE-RETORNADA                      PIC 9(03).
       05  95-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  95-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T95.
           10  95RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  95RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  95RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  95RT-CBCO                       PIC S9(3)V COMP-3.
           10  95RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  95RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  95RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  95RT-HULT-ATULZ                 PIC X(26).
           10  95RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  95RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  95RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  95RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  95RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.
           10  95RT-DMODIF-CHEQC-PCELA         PIC X(10).
           10  95RT-RMOTVO-DEVLC-CHEQC.
              49  95RT-RMOTVO-DEVLC-CHEQC-LEN  PIC S9(4) COMP.
              49  95RT-RMOTVO-DEVLC-CHEQC-TEXT PIC X(250).
           10  95RT-CINDCD-REAP-CHEQC          PIC S9(1)V COMP-3.
           10  95RT-DREAP-CHEQC                PIC X(10).
           10  95RT-CINDCD-MODLD-PROCM         PIC X(1).
           10  95RT-CINDCD-MOTVO-MODIF         PIC X(1).
           10  95RT-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
           10  95RT-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
           10  95RT-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
           10  95RT-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  95RT-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
           10  95RT-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
           10  95RT-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
           10  95RT-CCONTR-CORP-CHEQC          PIC S9(7)V COMP-3.
           10  95RT-DVNCLO-REAP-CHEQC          PIC X(10).
BSI001     10  95RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  95RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  95RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
