      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. RUEC7170.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: RUEC7170                                 *
      *    TELA.............: RUECT717                                 *
      *    PROGRAMADOR......: FABRICA SOFTWARE        - SONDA PROCWORK *
      *                                                                *
      *    ANALISTA.........: MARINA FUJITA                            *
      *    LIDER PROJETO....: MARINA FUJITA                            *
      *    GERENCIA / EQUIPE: GERENCIA-D / EQUIPE-D4  - SONDA PROCWORK *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP70                                 *
      *    ANALISTA.........:                                          *
      *                                                                *
      *    DATA.............: JUN/2012                                 *
      *                                                                *
      *    PROJETO..........: PWT-12/9931                              *
      *                       RUEC - CREDITO RURAL                     *
      *                                                                *
      *    OBJETIVO.........: EXIBIR DADOS PARA INCLUSAO RECOR -       *
      *                       DESTINACAO FINANCIAMENTO.                *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *         DB2PRD.TCONTR_ENVIO_RECOR        RUECB099              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *        BRAD0660 - COMPACTA DADOS PARA TRANSMISSAO              *
      *        POOL5005 - CONTROLA TRAFEGO DE MENSAGENS                *
      *        BRAD7100 - TRATA ERRO                                   *
      *                                                                *
      *    NAVEGACAO........:                                          *
      *        CHAMADO POR.: RUEC8330 - DADOS DE FINACIAMENTO          *
      *                                                                *
      *================================================================*
PRI499*                ALTERACAO - PRIME                               *
PRI499*----------------------------------------------------------------*
PRI499*                                                                *
PRI499* ANALISTA REPONSAVEL: WALTER MESSAS                             *
PRI499* DATA...............: 24/11/2022                                *
PRI499* OBJETIVOS..........: INCLUSAO DE TIPO DE CLIMA NO CODIGO       *
PRI499*                      DO SISTEMA DE PRODUCAO                    *
PRI499*================================================================*
           EJECT
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

           EJECT
      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** RUEC7170 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES ***'.
      *----------------------------------------------------------------*

       77  WRK-CPROG-RURAL-NULL        PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-CMUN-REFT-BACEN-NULL    PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-VPCELA-LIBRC-NULL       PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-VPCELA-REC-PPRIO-NULL   PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-QUND-FINCD-RURAL-NULL   PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-QUND-PROD-PROVV-NULL    PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-DINIC-SAFRA-NULL        PIC S9(004) COMP    VALUE ZEROS.
       77  WRK-DFIM-SAFRA-NULL         PIC S9(004) COMP    VALUE ZEROS.


       01  WRK-CABECALHO.
           05  WRK-CAB-CON             PIC  9(009)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-CAB-AGE             PIC  9(005)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE '/'.
           05  WRK-CAB-CED             PIC  9(009)         VALUE ZEROS.

       01  WRK-ANO-INIFIM.
           05  WRK-ANO-INI             PIC  9(004)         VALUE ZEROS.
           05  WRK-ANO-FIM             PIC  9(004)         VALUE ZEROS.

       01  WRK-TAM-02-COM-S            PIC -9(002)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-02-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-02-SEM-S        PIC  9(002).

       01  WRK-TAM-06-COM-S            PIC -9(006)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-06-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-06-SEM-S        PIC  9(006).

       01  WRK-TAM-03V2-COM-S          PIC -9(003)V9(2)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-03V2-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-X.
             10  WRK-TAM-03V2-SEM-S    PIC  9(003)V9(2).

       01  WRK-TAM-03V5-COM-S          PIC -9(003)V9(5)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-03V5-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-05-X1.
             10  FILLER                PIC  9(003)V9(2).
           05  FILLER                  PIC  X(003).

       01  WRK-TAM-13V2-COM-S          PIC -9(013)V9(2)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-13V2-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-15-X.
             10  WRK-TAM-13V2-SEM-S    PIC  9(013)V9(2).

       01  WRK-TAM-07V2-COM-S          PIC -9(007)V9(2)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-07V2-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-09-X.
             10  WRK-TAM-07V2-SEM-S    PIC  9(007)V9(2).

       01  WRK-TAM-09V2-COM-S          PIC -9(009)V9(2)    VALUE ZEROS.
       01  FILLER REDEFINES WRK-TAM-09V2-COM-S.
           05  FILLER                  PIC  X(001).
           05  WRK-TAM-11-X.
             10  WRK-TAM-09V2-SEM-S    PIC  9(009)V9(2).

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREAS DA POOL5005 ***'.
      *----------------------------------------------------------------*

       77  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       77  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       77  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       77  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.

       77  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       77  WRK-NOVA-OPCAO              PIC  9(002)         VALUE ZEROS.
       77  WRK-OPCAO-ATUAL             PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-USER.
           05  WRK-COD-USER-N          PIC  9(007)         VALUE ZEROS.

       01  WRK-COD-DEPTO.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  WRK-COD-DEPTO-N         PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05  FILLER                  PIC  X(004)         VALUE SPACES.
           05  WRK-TRANSACAO           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA MENSAGENS ***'.
      *----------------------------------------------------------------*

       01  WRK-MSG001                  PIC  X(079)         VALUE
           'PF INVALIDA'.

       01  WRK-MSG002                  PIC  X(079)         VALUE
           'TECLE PF8 PARA AVANCAR'.

       01  WRK-MSG003                  PIC  X(079)         VALUE
           'CONTRATO INEXISTENTE'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)         VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660 '.
           05  FILLER                  PIC  X(012)         VALUE
               '- RET.COD = '.
           05  WRK-RETURN-CODE         PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(011)         VALUE
               ' - LOCAL = '.
           05  WRK-LOCAL-ERRO          PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TELA RUECT717          -  INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           05  FILLER                  PIC  X(018)         VALUE SPACES.
           05  WRK-INP-PFK             PIC  X(001)         VALUE SPACES.
           05  WRK-INP-SENHAS          PIC  X(037)         VALUE SPACES.
           05  WRK-INP-NOPCAO          PIC  X(002)         VALUE SPACES.
           05  WRK-INP-CPOESC.
             10  WRK-INP-AREA-ESC      PIC  X(046)         VALUE SPACES.
           05  WRK-INP-DADOS.
             10  WRK-INP-INFCAB        PIC  X(025)         VALUE SPACES.
             10  WRK-INP-CODEMP        PIC  X(014)         VALUE SPACES.
PRI499       10  WRK-INP-CODSIS        PIC  X(011)         VALUE SPACES.
             10  WRK-INP-VLRCRE        PIC  X(015)         VALUE SPACES.
             10  WRK-INP-VLRREC        PIC  X(015)         VALUE SPACES.
             10  WRK-INP-ALIPRO        PIC  X(005)         VALUE SPACES.
             10  WRK-INP-JURENC        PIC  X(005)         VALUE SPACES.
             10  WRK-INP-TIPENC        PIC  X(001)         VALUE SPACES.
             10  WRK-INP-CODSTN        PIC  X(015)         VALUE SPACES.
             10  WRK-INP-RISSTN        PIC  X(005)         VALUE SPACES.
             10  WRK-INP-RISFUN        PIC  X(005)         VALUE SPACES.
             10  WRK-INP-QTAREA        PIC  X(009)         VALUE SPACES.
             10  WRK-INP-QTITEM        PIC  X(011)         VALUE SPACES.
             10  WRK-INP-QTPREV        PIC  X(011)         VALUE SPACES.
             10  WRK-INP-IDESAF        PIC  X(008)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** TELA RUECT717          -  WRK-OUTPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05  WRK-OUT-LL              PIC S9(004) COMP    VALUE +0.
           05  WRK-OUT-ZZ              PIC  9(004) COMP    VALUE  0.
           05  WRK-OUT-SENHAS          PIC  X(037)         VALUE SPACES.
           05  WRK-OUT-NOPCAO          PIC  X(002)         VALUE SPACES.
           05  WRK-OUT-CPOESC.
             10  WRK-OUT-AREA-ESC      PIC  X(046)         VALUE SPACES.
           05  WRK-OUT-DADOS.
             10  WRK-OUT-INFCAB        PIC  X(025)         VALUE SPACES.
             10  WRK-OUT-CODEMP        PIC  X(014)         VALUE SPACES.
PRI499       10  WRK-OUT-CODSIS        PIC  X(011)         VALUE SPACES.
             10  WRK-OUT-VLRCRE        PIC  X(015)         VALUE SPACES.
             10  WRK-OUT-VLRREC        PIC  X(015)         VALUE SPACES.
             10  WRK-OUT-ALIPRO.
               15  WRK-OUT-ALIPRO-N    PIC  9(005)         VALUE ZEROS.
             10  WRK-OUT-JURENC        PIC  X(005)         VALUE SPACES.
             10  WRK-OUT-TIPENC.
               15  WRK-OUT-TIPENC-N    PIC  9(002)         VALUE ZEROS.
             10  WRK-OUT-CODSTN        PIC  X(015)         VALUE SPACES.
             10  WRK-OUT-RISSTN.
               15  WRK-OUT-RISSTN-N    PIC  9(005)         VALUE ZEROS.
             10  WRK-OUT-RISFUN.
               15  WRK-OUT-RISFUN-N    PIC  9(005)         VALUE ZEROS.
             10  WRK-OUT-QTAREA.
               15  WRK-OUT-QTAREA-N    PIC  9(009)         VALUE ZEROS.
             10  WRK-OUT-QTITEM.
               15  WRK-OUT-QTITEM-N    PIC  9(011)         VALUE ZEROS.
             10  WRK-OUT-QTPREV.
               15  WRK-OUT-QTPREV-N    PIC  9(011)         VALUE ZEROS.
             10  WRK-OUT-IDESAF.
               15  WRK-OUT-IDESAF-N    PIC  9(008)         VALUE ZEROS.
             10  WRK-OUT-MENSAG        PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD0660 DA TELA RUECT717 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUECT717.
           05  WRK-660-LL-AREA         PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSAGEM     PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS          PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-NOPCAO          PIC  9(004) COMP    VALUE 0002.
           05  WRK-660-CPOESC          PIC  9(004) COMP    VALUE 0046.
           05  WRK-660-INFCAB          PIC  9(004) COMP    VALUE 0025.
           05  WRK-660-CODEMP          PIC  9(004) COMP    VALUE 0014.
PRI499     05  WRK-660-CODSIS          PIC  9(004) COMP    VALUE 0011.
           05  WRK-660-VLRCRE          PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-VLRREC          PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-ALIPRO          PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-JURENC          PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-TIPENC          PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-CODSTN          PIC  9(004) COMP    VALUE 0015.
           05  WRK-660-RISSTN          PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-RISFUN          PIC  9(004) COMP    VALUE 0005.
           05  WRK-660-QTAREA          PIC  9(004) COMP    VALUE 0009.
           05  WRK-660-QTITEM          PIC  9(004) COMP    VALUE 0011.
           05  WRK-660-QTPREV          PIC  9(004) COMP    VALUE 0011.
           05  WRK-660-IDESAF          PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-MENSAG          PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP    VALUE +104.
           05  COMU-ZZ                 PIC  9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE           PIC  X(015)         VALUE SPACES.
           05  COMU-SENHA              PIC  X(037)         VALUE SPACES.
           05  COMU-NOPCAO             PIC  X(002)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-CCONTR         PIC  9(009)         VALUE ZEROS.
               10  COMU-NENVIO         PIC  9(005)         VALUE ZEROS.
               10  COMU-CJUNC          PIC  9(005)         VALUE ZEROS.
               10  COMU-CCDULA         PIC  9(009)         VALUE ZEROS.
               10  COMU-PAGINA         PIC  9(003)         VALUE ZEROS.
               10  COMU-CHVPES         PIC  X(015)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA TABELA DB2 ***'.
      *----------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE RUECB099
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** RUEC7170 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  LNK-IO-STATUS           PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  LNK-IO-MODNAME          PIC  X(008)         VALUE SPACES.

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008)         VALUE SPACES.
           05  FILLER                  PIC  X(002)         VALUE SPACES.
           05  LNK-ALT-STATUS          PIC  X(002)         VALUE SPACES.
           05  FILLER                  PIC  X(012)         VALUE SPACES.
           05  LNK-ALT-MODNAME         PIC  X(008)         VALUE SPACES.

           EJECT
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'          USING  LNK-IO-PCB
                                           LNK-ALT-PCB.

           MOVE    WRK-GU              TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
      *    ROTINA INICIAR
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING   WRK-FUNCAO
                                               LNK-IO-PCB
                                               LNK-ALT-PCB
                                               WRK-MENSAGEM
                                               WRK-TELA
                                               WRK-NOVA-OPCAO
                                               WRK-COD-USER
                                               WRK-OPCAO-ATUAL
                                               WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               GOBACK
           END-IF.

           MOVE SPACES                 TO  WRK-OUT-DADOS
                                           WRK-INP-DADOS

           MOVE 'RUECT717'             TO  WRK-TELA.

           IF  WRK-TRANSACAO           NOT EQUAL 'RUEC7170'
               PERFORM 1000-RECEBER-CONTROLE
           ELSE
               PERFORM 2000-PROCESSAR-RUECT717
           END-IF.

           IF  WRK-TELA                EQUAL  'RUECT717'
               PERFORM 3000-PROCESSAR-BRAD0660
           END-IF.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RECEBER-CONTROLE
      *----------------------------------------------------------------*
       1000-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM(1:LENGTH OF COMU-AREA)
                                       TO  COMU-AREA

           MOVE COMU-DADOS             TO  WRK-INP-AREA-ESC
                                           WRK-OUT-AREA-ESC

           PERFORM 1100-MONTAR-TELA

           IF  WRK-OUT-INFCAB          EQUAL SPACES
               MOVE WRK-MSG003         TO  WRK-OUT-MENSAG
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA MONTAR-TELA
      *----------------------------------------------------------------*
       1100-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           MOVE COMU-CCONTR            TO  WRK-CAB-CON
           MOVE COMU-CJUNC             TO  WRK-CAB-AGE
           MOVE COMU-CCDULA            TO  WRK-CAB-CED
           MOVE WRK-CABECALHO          TO  WRK-OUT-INFCAB

           PERFORM 1110-ACESSAR-RUECB099

           IF  SQLCODE                 EQUAL +100
               GO                      TO  1100-99-FIM
           END-IF.


      *--> FORMATAR OS CAMPOS DA TELA

           MOVE CEMPTO-BACEN           TO  WRK-OUT-CODEMP
           MOVE CSIST-PROD             TO  WRK-OUT-CODSIS
           IF  WRK-OUT-CODSIS(10:02)  EQUAL SPACES OR LOW-VALUES
               MOVE '00'           TO WRK-OUT-CODSIS(10:02)
           END-IF

           MOVE VPCELA-LIBRC           TO  WRK-TAM-13V2-COM-S
           MOVE WRK-TAM-15-X           TO  WRK-OUT-VLRCRE

           MOVE VPCELA-REC-PPRIO       TO  WRK-TAM-13V2-COM-S
           MOVE WRK-TAM-15-X           TO  WRK-OUT-VLRREC

           MOVE PALIQT-PROAGRO         TO  WRK-TAM-03V2-COM-S
           MOVE WRK-TAM-05-X           TO  WRK-OUT-ALIPRO

           MOVE PJURO-ENCAR-FINCR      TO  WRK-TAM-03V5-COM-S
           MOVE WRK-TAM-05-X1          TO  WRK-OUT-JURENC

           MOVE CTPO-ENCAR-COMPL       TO  WRK-TAM-02-COM-S
           MOVE WRK-TAM-02-SEM-S       TO  WRK-OUT-TIPENC-N

           MOVE CTSORO-NACIO           TO  WRK-OUT-CODSTN

           MOVE PRISCO-TSORO-NACIO     TO  WRK-TAM-03V2-COM-S
           MOVE WRK-TAM-05-X           TO  WRK-OUT-RISSTN

           MOVE PRISCO-FUNDO-CNSTT     TO  WRK-TAM-03V2-COM-S
           MOVE WRK-TAM-05-X           TO  WRK-OUT-RISFUN

           IF  QAREA-RECOR             EQUAL ZEROS
               MOVE SPACES             TO  WRK-OUT-QTAREA
           ELSE
               MOVE QAREA-RECOR        TO  WRK-TAM-07V2-COM-S
               MOVE WRK-TAM-09-X       TO  WRK-OUT-QTAREA
           END-IF.

           IF  QUND-FINCD-RURAL        EQUAL ZEROS
               MOVE SPACES             TO  WRK-OUT-QTITEM
           ELSE
               MOVE QUND-FINCD-RURAL   TO  WRK-TAM-09V2-COM-S
               MOVE WRK-TAM-11-X       TO  WRK-OUT-QTITEM
           END-IF.

           MOVE QUND-PROD-PROVV        TO  WRK-TAM-09V2-COM-S
           MOVE WRK-TAM-11-X           TO  WRK-OUT-QTPREV

           MOVE DINIC-SAFRA            TO  WRK-TAM-06-COM-S
           MOVE WRK-TAM-06-SEM-S(3:4)  TO  WRK-ANO-INI
           MOVE DFIM-SAFRA             TO  WRK-TAM-06-COM-S
           MOVE WRK-TAM-06-SEM-S(3:4)  TO  WRK-ANO-FIM
           MOVE WRK-ANO-INIFIM         TO  WRK-OUT-IDESAF

           MOVE WRK-MSG002             TO  WRK-OUT-MENSAG.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA ACESSAR-RUECB099
      *----------------------------------------------------------------*
       1110-ACESSAR-RUECB099           SECTION.
      *----------------------------------------------------------------*

           MOVE COMU-CCONTR            TO  CCONTR-CREDT-RURAL
                                                           OF RUECB099
           MOVE COMU-NENVIO            TO  NENVIO-RECOR    OF RUECB099

           EXEC SQL
               SELECT
                   CPROG_RURAL         ,
                   CTPO_FONTE_RECOR    ,
                   CMUN_REFT_BACEN     ,
                   CEMPTO_BACEN        ,
                   CSIST_PROD          ,
                   VPCELA_LIBRC        ,
                   VPCELA_REC_PPRIO    ,
                   PALIQT_PROAGRO      ,
                   PJURO_ENCAR_FINCR   ,
                   CTPO_ENCAR_COMPL    ,
                   CTSORO_NACIO        ,
                   PRISCO_TSORO_NACIO  ,
                   PRISCO_FUNDO_CNSTT  ,
                   QAREA_RECOR         ,
                   QUND_FINCD_RURAL    ,
                   QUND_PROD_PROVV     ,
                   DINIC_SAFRA         ,
                   DFIM_SAFRA
               INTO
                   :RUECB099.CPROG-RURAL
                        :WRK-CPROG-RURAL-NULL      ,
                   :RUECB099.CTPO-FONTE-RECOR      ,
                   :RUECB099.CMUN-REFT-BACEN
                        :WRK-CMUN-REFT-BACEN-NULL  ,
                   :RUECB099.CEMPTO-BACEN          ,
                   :RUECB099.CSIST-PROD            ,
                   :RUECB099.VPCELA-LIBRC
                        :WRK-VPCELA-LIBRC-NULL     ,
                   :RUECB099.VPCELA-REC-PPRIO
                        :WRK-VPCELA-REC-PPRIO-NULL ,
                   :RUECB099.PALIQT-PROAGRO        ,
                   :RUECB099.PJURO-ENCAR-FINCR     ,
                   :RUECB099.CTPO-ENCAR-COMPL      ,
                   :RUECB099.CTSORO-NACIO          ,
                   :RUECB099.PRISCO-TSORO-NACIO    ,
                   :RUECB099.PRISCO-FUNDO-CNSTT    ,
                   :RUECB099.QAREA-RECOR           ,
                   :RUECB099.QUND-FINCD-RURAL
                        :WRK-QUND-FINCD-RURAL-NULL ,
                   :RUECB099.QUND-PROD-PROVV
                        :WRK-QUND-PROD-PROVV-NULL  ,
                   :RUECB099.DINIC-SAFRA
                        :WRK-DINIC-SAFRA-NULL      ,
                   :RUECB099.DFIM-SAFRA
                        :WRK-DFIM-SAFRA-NULL
               FROM
                   DB2PRD.TCONTR_ENVIO_RECOR
               WHERE
                   CCONTR_CREDT_RURAL  = :RUECB099.CCONTR-CREDT-RURAL
               AND NENVIO_RECOR        = :RUECB099.NENVIO-RECOR
           END-EXEC.

           IF (SQLCODE   NOT  EQUAL ZEROS AND +100 )   OR
              (SQLWARN0       EQUAL 'W'   )
               MOVE 'DB2'              TO  ERR-TIPO-ACESSO
               MOVE 'TCONTR_ENVIO_RECOR' TO  ERR-DBD-TAB
               MOVE 'SELECT'           TO  ERR-FUN-COMANDO
               MOVE SQLCODE            TO  ERR-SQL-CODE
               MOVE '0001'             TO  ERR-LOCAL
               MOVE SPACES             TO  ERR-SEGM
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL ZEROS
               IF  WRK-CPROG-RURAL-NULL
                                       EQUAL -1
                   MOVE SPACES         TO  CPROG-RURAL OF RUECB099
               END-IF
               IF  WRK-CMUN-REFT-BACEN-NULL
                                       EQUAL -1
                   MOVE ZEROS          TO  CMUN-REFT-BACEN OF RUECB099
               END-IF
               IF  WRK-VPCELA-LIBRC-NULL
                                       EQUAL -1
                   MOVE ZEROS          TO  VPCELA-LIBRC OF RUECB099
               END-IF
               IF  WRK-VPCELA-REC-PPRIO-NULL
                                       EQUAL -1
                   MOVE ZEROS          TO  VPCELA-REC-PPRIO OF RUECB099
               END-IF
               IF  WRK-QUND-FINCD-RURAL-NULL
                                       EQUAL -1
                   MOVE ZEROS          TO  QUND-FINCD-RURAL OF RUECB099
               END-IF
               IF  WRK-QUND-PROD-PROVV-NULL
                                       EQUAL -1
                   MOVE ZEROS          TO  QUND-PROD-PROVV OF RUECB099
               END-IF
               IF  WRK-DINIC-SAFRA-NULL
                                       EQUAL -1
                   MOVE ZEROS          TO  DINIC-SAFRA OF RUECB099
               END-IF
               IF  WRK-DFIM-SAFRA-NULL EQUAL -1
                   MOVE ZEROS          TO  DFIM-SAFRA OF RUECB099
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR-RUECT717
      *----------------------------------------------------------------*
       2000-PROCESSAR-RUECT717         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM(1:LENGTH OF WRK-INPUT) TO WRK-INPUT.

           PERFORM 2100-DEVOLVER-TELA.


      *--> TRATA PFS VALIDAS PARA TODAS AS FASES

           EVALUATE TRUE
             WHEN  WRK-INP-PFK         EQUAL 'A' OR '7'
               PERFORM 2200-PROCESSAR-RETORNO
               GO                      TO  2000-99-FIM

             WHEN  WRK-INP-PFK         EQUAL 'B'
               PERFORM 2400-PROCESSAR-CHAMADA
               GO                      TO  2000-99-FIM
           END-EVALUATE.


      *--> TRATAR AS FASES

           PERFORM 2300-PROCESSAR-FASE-1.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DEVOLVER-TELA
      *----------------------------------------------------------------*
       2100-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-CPOESC         TO  WRK-OUT-CPOESC
           MOVE WRK-INP-INFCAB         TO  WRK-OUT-INFCAB
           MOVE WRK-INP-CODEMP         TO  WRK-OUT-CODEMP
           MOVE WRK-INP-CODSIS         TO  WRK-OUT-CODSIS
           MOVE WRK-INP-VLRCRE         TO  WRK-OUT-VLRCRE
           MOVE WRK-INP-VLRREC         TO  WRK-OUT-VLRREC
           MOVE WRK-INP-ALIPRO         TO  WRK-OUT-ALIPRO
           MOVE WRK-INP-JURENC         TO  WRK-OUT-JURENC
           MOVE WRK-INP-TIPENC         TO  WRK-OUT-TIPENC
           MOVE WRK-INP-CODSTN         TO  WRK-OUT-CODSTN
           MOVE WRK-INP-RISSTN         TO  WRK-OUT-RISSTN
           MOVE WRK-INP-RISFUN         TO  WRK-OUT-RISFUN
           MOVE WRK-INP-QTAREA         TO  WRK-OUT-QTAREA
           MOVE WRK-INP-QTITEM         TO  WRK-OUT-QTITEM
           MOVE WRK-INP-QTPREV         TO  WRK-OUT-QTPREV
           MOVE WRK-INP-IDESAF         TO  WRK-OUT-IDESAF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR-RETORNO
      *----------------------------------------------------------------*
       2200-PROCESSAR-RETORNO          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA
           MOVE LENGTH OF COMU-AREA    TO  COMU-LL
           MOVE 'RUEC7170'             TO  COMU-TRANCODE
           MOVE WRK-INP-AREA-ESC       TO  COMU-DADOS

           MOVE COMU-AREA              TO  WRK-MENSAGEM.
           MOVE WRK-CHNG               TO  WRK-FUNCAO.
           MOVE 'RUEC8330'             TO  WRK-TELA.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR-FASE-1
      *----------------------------------------------------------------*
       2300-PROCESSAR-FASE-1           SECTION.
      *----------------------------------------------------------------*

           EVALUATE TRUE
               WHEN WRK-INP-PFK        EQUAL '8'
                    PERFORM 2310-TRATAR-PFK08

               WHEN OTHER
                    MOVE WRK-MSG001    TO  WRK-OUT-MENSAG
           END-EVALUATE.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA TRATAR-PFK08
      *----------------------------------------------------------------*
       2310-TRATAR-PFK08               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA
           MOVE LENGTH OF COMU-AREA    TO  COMU-LL
           MOVE 'RUEC7170'             TO  COMU-TRANCODE
           MOVE WRK-INP-AREA-ESC       TO  COMU-DADOS

           MOVE COMU-AREA              TO  WRK-MENSAGEM.
           MOVE WRK-CHNG               TO  WRK-FUNCAO.
           MOVE 'RUEC8340'             TO  WRK-TELA.

      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR-CHAMADA
      *----------------------------------------------------------------*
       2400-PROCESSAR-CHAMADA          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA
           MOVE +72                    TO  COMU-LL
           MOVE 'RUEC7170'             TO  COMU-TRANCODE
           MOVE WRK-INP-AREA-ESC       TO  COMU-DADOS

           MOVE COMU-AREA              TO  WRK-MENSAGEM.
           MOVE WRK-CHNG               TO  WRK-FUNCAO.
           MOVE 'RUEC9185'             TO  WRK-TELA.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR-BRAD0660
      *----------------------------------------------------------------*
       3000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT   TO  WRK-OUT-LL
                                           WRK-660-LL-MENSAGEM.
           MOVE LENGTH OF WRK-660-RUECT717
                                       TO  WRK-660-LL-AREA.

           CALL 'BRAD0660'          USING  WRK-OUTPUT
                                           WRK-660-RUECT717.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE  RETURN-CODE       TO  WRK-RETURN-CODE
               MOVE  20                TO  WRK-LOCAL-ERRO
               MOVE  WRK-ERRO-BRAD0660 TO  ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO  WRK-FUNCAO.
           MOVE WRK-OUTPUT             TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA PROCESSAR-ROTINA-ERRO
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           MOVE    'SENHAS02'          TO  ERR-PGM.
           MOVE    'RUEC7170'          TO  ERR-MODULO.
           MOVE    WRK-COD-USER        TO  ERR-COD-USER.
           MOVE    WRK-COD-DEPTO       TO  ERR-COD-DEPTO.

           CALL    'BRAD7100'        USING LNK-IO-PCB
                                           ERRO-AREA
                                           LNK-ALT-PCB
                                           SQLCA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
