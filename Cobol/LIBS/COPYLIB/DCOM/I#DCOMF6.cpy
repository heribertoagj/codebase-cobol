      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F6 - DB2PRD.TTRASI_CHEQC_PCELA     *
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
       01  F6-ARGUMENTOS-ENTRADA.

       05  F6-COLUNAS-DA-TABELA.
           10  F6-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F6-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F6-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  F6-CBCO                         PIC S9(3)V COMP-3.
           10  F6-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  F6-CCTA-CORR-BCRIA              PIC S9(11)V COMP-3.
           10  F6-CCHEQ                        PIC S9(7)V COMP-3.
           10  F6-HULT-ATULZ                   PIC X(26).
           10  F6-CDEPDC                       PIC S9(5)V COMP-3.
           10  F6-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  F6-CFLIAL-EMPR-CHEQC            PIC S9(5)V COMP-3.
           10  F6-DVCTO-CHEQ-CTDIA             PIC X(10).
           10  F6-VCHEQC                       PIC S9(13)V9(2) COMP-3.
           10  F6-DMODIF-CHEQC-PCELA           PIC X(10).
           10  F6-RMOTVO-DEVLC-CHEQC.
              49  F6-RMOTVO-DEVLC-CHEQC-LEN    PIC S9(4) COMP.
              49  F6-RMOTVO-DEVLC-CHEQC-TEXT   PIC X(250).
           10  F6-CINDCD-REAP-CHEQC            PIC S9(1)V COMP-3.
           10  F6-DREAP-CHEQC                  PIC X(10).
           10  F6-CINDCD-MODLD-PROCM           PIC X(1).
           10  F6-CINDCD-MOTVO-MODIF           PIC X(1).
           10  F6-VSDO-DVDOR-PRINC             PIC S9(15)V9(2) COMP-3.
           10  F6-VJURO-MORA-LEGAL             PIC S9(15)V9(2) COMP-3.
           10  F6-VJURO-MORA-GERC              PIC S9(15)V9(2) COMP-3.
           10  F6-VIOF-PCELA-VENCD             PIC S9(15)V9(2) COMP-3.
           10  F6-VCUSTO-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  F6-VJURO-TRNSF-CRATS            PIC S9(15)V9(2) COMP-3.
           10  F6-VCUSTO-TRNSF-CRATS           PIC S9(15)V9(2) COMP-3.
           10  F6-CCONTR-CORP-CHEQC            PIC S9(7)V COMP-3.
           10  F6-DVNCLO-REAP-CHEQC            PIC X(10).
BSI001     10  F6-VJURO-REMUN-VENCD            PIC S9(15)V9(2) COMP-3.
BSI001     10  F6-VMORA-VENCD-DESC             PIC S9(15)V9(2) COMP-3.
BSI001     10  F6-VMULTA-VENCD-DESC            PIC S9(15)V9(2) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F6-RETORNO.
       05  F6-COD-RETORNO                         PIC 9(04).
       05  F6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F6-PROXIMO-RESTART                     PIC 9(05).
       05  F6-QTDE-RETORNADA                      PIC 9(03).
       05  F6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF6.
           10  F6RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F6RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F6RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  F6RT-CBCO                       PIC S9(3)V COMP-3.
           10  F6RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  F6RT-CCTA-CORR-BCRIA            PIC S9(11)V COMP-3.
           10  F6RT-CCHEQ                      PIC S9(7)V COMP-3.
           10  F6RT-HULT-ATULZ                 PIC X(26).
           10  F6RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  F6RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  F6RT-CFLIAL-EMPR-CHEQC          PIC S9(5)V COMP-3.
           10  F6RT-DVCTO-CHEQ-CTDIA           PIC X(10).
           10  F6RT-VCHEQC                     PIC S9(13)V9(2) COMP-3.
           10  F6RT-DMODIF-CHEQC-PCELA         PIC X(10).
           10  F6RT-RMOTVO-DEVLC-CHEQC.
              49  F6RT-RMOTVO-DEVLC-CHEQC-LEN  PIC S9(4) COMP.
              49  F6RT-RMOTVO-DEVLC-CHEQC-TEXT PIC X(250).
           10  F6RT-CINDCD-REAP-CHEQC          PIC S9(1)V COMP-3.
           10  F6RT-DREAP-CHEQC                PIC X(10).
           10  F6RT-CINDCD-MODLD-PROCM         PIC X(1).
           10  F6RT-CINDCD-MOTVO-MODIF         PIC X(1).
           10  F6RT-VSDO-DVDOR-PRINC           PIC S9(15)V9(2) COMP-3.
           10  F6RT-VJURO-MORA-LEGAL           PIC S9(15)V9(2) COMP-3.
           10  F6RT-VJURO-MORA-GERC            PIC S9(15)V9(2) COMP-3.
           10  F6RT-VIOF-PCELA-VENCD           PIC S9(15)V9(2) COMP-3.
           10  F6RT-VCUSTO-PCELA-VENCD         PIC S9(15)V9(2) COMP-3.
           10  F6RT-VJURO-TRNSF-CRATS          PIC S9(15)V9(2) COMP-3.
           10  F6RT-VCUSTO-TRNSF-CRATS         PIC S9(15)V9(2) COMP-3.
           10  F6RT-CCONTR-CORP-CHEQC          PIC S9(7)V COMP-3.
           10  F6RT-DVNCLO-REAP-CHEQC          PIC X(10).
BSI001     10  F6RT-VJURO-REMUN-VENCD          PIC S9(15)V9(2) COMP-3.
BSI001     10  F6RT-VMORA-VENCD-DESC           PIC S9(15)V9(2) COMP-3.
BSI001     10  F6RT-VMULTA-VENCD-DESC          PIC S9(15)V9(2) COMP-3.
