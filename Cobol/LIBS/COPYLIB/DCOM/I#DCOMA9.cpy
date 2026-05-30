      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A9 - DB2PRD.THIST_CHEQC_PCELA      *
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
       01  A9-ARGUMENTOS-ENTRADA.

       05  A9-COLUNAS-DA-TABELA.
           10  A9-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  A9-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  A9-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  A9-CBCO                         PIC S9(3)V COMP-3.
           10  A9-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  A9-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  A9-CCHEQ                        PIC S9(7)V COMP-3.
           10  A9-HULT-ATULZ                   PIC X(26).
           10  A9-CDEPDC                       PIC S9(5)V COMP-3.
           10  A9-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  A9-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  A9-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  A9-VCHEQC                       PIC S9(13)V9(2) COMP-3.
           10  A9-DMODIF-CHEQC-PCELA           PIC X(10).
           10  A9-RMOTVO-DEVLC-CHEQC.
              49  A9-RMOTVO-DEVLC-CHEQC-LEN    PIC S9(4) COMP.
              49  A9-RMOTVO-DEVLC-CHEQC-TEXT   PIC X(250).
           10  A9-CINDCD-REAP-CHEQC            PIC S9(1)V COMP-3.
           10  A9-DREAP-CHEQC                  PIC X(10).
           10  A9-CINDCD-MODLD-PROCM           PIC X(1).
           10  A9-CINDCD-MOTVO-MODIF           PIC X(1).
           10  A9-VSDO-DVDOR-PRINC             PIC S9(15)V9(2) COMP-3.
           10  A9-VJURO-MORA-LEGAL             PIC S9(15)V9(2) COMP-3.
           10  A9-VJURO-MORA-GERC              PIC S9(15)V9(2) COMP-3.
           10  A9-VIOF-PCELA-VENCD             PIC S9(15)V9(2) COMP-3.
           10  A9-VCUSTO-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  A9-VJURO-TRNSF-CRATS            PIC S9(15)V9(2) COMP-3.
           10  A9-VCUSTO-TRNSF-CRATS           PIC S9(15)V9(2) COMP-3.
           10  A9-CCONTR-CORP-CHEQC            PIC S9(7)V COMP-3.
           10  A9-DVNCLO-REAP-CHEQC            PIC X(10).
BSI001     10  A9-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  A9-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  A9-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A9-RETORNO.
       05  A9-COD-RETORNO                         PIC 9(04).
       05  A9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A9-PROXIMO-RESTART                     PIC 9(05).
       05  A9-QTDE-RETORNADA                      PIC 9(03).
       05  A9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA9.
           10  A9RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  A9RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  A9RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  A9RT-CBCO                       PIC S9(3)V COMP-3.
           10  A9RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  A9RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  A9RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  A9RT-HULT-ATULZ                 PIC X(26).
           10  A9RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  A9RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  A9RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  A9RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  A9RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.
           10  A9RT-DMODIF-CHEQC-PCELA         PIC X(10).
           10  A9RT-RMOTVO-DEVLC-CHEQC.
              49  A9RT-RMOTVO-DEVLC-CHEQC-LEN  PIC S9(4) COMP.
              49  A9RT-RMOTVO-DEVLC-CHEQC-TEXT PIC X(250).
           10  A9RT-CINDCD-REAP-CHEQC          PIC S9(1)V COMP-3.
           10  A9RT-DREAP-CHEQC                PIC X(10).
           10  A9RT-CINDCD-MODLD-PROCM         PIC X(1).
           10  A9RT-CINDCD-MOTVO-MODIF         PIC X(1).
           10  A9RT-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
           10  A9RT-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
           10  A9RT-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
           10  A9RT-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  A9RT-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
           10  A9RT-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
           10  A9RT-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
           10  A9RT-CCONTR-CORP-CHEQC          PIC S9(7)V COMP-3.
           10  A9RT-DVNCLO-REAP-CHEQC          PIC X(10).
BSI001     10  A9RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  A9RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  A9RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
