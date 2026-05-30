      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     RUCA7951.
       AUTHOR.         HERIBERTO GIANNASI.
      *================================================================*
      *                         7 C O M M                              *
      *----------------------------------------------------------------*
      *     PROGRAMA....: RUCA7951                                     *
      *     PROGRAMADOR.: HERIBERTO GIANNASI                           *
      *     ANALISTA....: HERIBERTO GIANNASI                           *
      *     DATA........: 01/2026                                      *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTAR CONTRATOS PARA RENEGOCIACAO           *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUCAT797....: LISTAR CONTRATOS PARA RENEGOCIACAO           *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *================================================================*
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *================================================================*
       DATA                             DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '** RUCA7951 - AREA DE WORKING **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DE ATRIBUTOS        ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           03  WRK-ATRIBUTOS.

      *****************************************************************
      *    PROTEGIDO (NUMERICO OU ALFA)                               *
      *               -------- -- ----                                *
               05  WRK-225             PIC S9(008) COMP VALUE +225.
      *****************************************************************
      *    BRILHANTE, DESPROT, COM POS. CURSOR, ALFA                  *
      *                                         ----                  *
      *****************************************************************
      *    PROTEGIDO, BRILHANTE (NUMERICO OU ALFA)                    *
      *                          -------- -- ----                     *
               05  WRK-169             PIC S9(008) COMP VALUE +169.
      *****************************************************************

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** VARIAVEIS AUXILIARES     ***'.
      *----------------------------------------------------------------*
       01 WRK-PAGINA                    PIC  9(02)  COMP-3 VALUE ZEROS.

       01 WRK-INCONSIS                  PIC  X(001)    VALUE SPACES.

       01 IND-1                         PIC  9(003) COMP-3
                                                       VALUE ZEROS.
       01 WRK-FIM-CSR                   PIC  X(001)    VALUE SPACES.

       01 WRK-VALOR-TOTAL               PIC  9(15)V99 COMP-3
                                                       VALUE ZEROS.

       01 WRK-S9-09                     PIC +9(009)    VALUE ZEROS.
       01 FILLER                        REDEFINES WRK-S9-09.
          03 FILLER                     PIC  X(01).
          03 WRK-9-09                   PIC  9(009).

       01 WRK-S9-07                     PIC +9(007)    VALUE ZEROS.
       01 FILLER                        REDEFINES WRK-S9-07.
          03 FILLER                     PIC  X(01).
          03 WRK-9-07                   PIC  9(007).

       77 WRK-QTD-CONTR                 PIC  9(01)     COMP-3
                                                       VALUE ZEROS.

       01 WRK-S9-15V2                   PIC +9(15)V99  VALUE ZEROS.
       01 FILLER REDEFINES WRK-S9-15V2.
          05 FILLER                     PIC  X(01).
          05 WRK-9-15V2                 PIC  9(15)V99.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA POOL5000         ***'.
      *----------------------------------------------------------------*

       01  WRK-CNPJ.
           05 WRK-CNPJ-CGC               PIC  ZZZZZZZZ9 VALUE ZEROS.
           05 FILLER                     PIC  X(01)     VALUE '/'.
           05 WRK-CNPJ-FILIAL            PIC  ZZZ9      VALUE ZEROS.
           05 FILLER                     PIC  X(01)     VALUE '-'.
           05 WRK-CNPJ-CTRL              PIC  9(02)     VALUE ZEROS.

       01  WRK-CPF.
           05 WRK-CPF-CGC                PIC  ZZZZZZZZ9 VALUE ZEROS.
           05 FILLER                     PIC  X(01)     VALUE '-'.
           05 WRK-CPF-CTRL               PIC  9(02)     VALUE ZEROS.

       01  WRK-S9-15                     PIC +9(15)     VALUE ZEROS.
       01  FILLER                        REDEFINES WRK-S9-15.
           05 FILLER                     PIC X(01).
           05 WRK-9-15-X.
              10 WRK-9-15                   PIC 9(15).

       01  WRK-CNPJ-CPF.
           05 WRK-CNPJ-CPF-CGC           PIC 9(09)      VALUE ZEROS.
           05 WRK-CNPJ-CPF-FILIAL        PIC 9(04)      VALUE ZEROS.
           05 WRK-CNPJ-CPF-CTRL          PIC 9(02)      VALUE ZEROS.

       01  WRK-GU                        PIC  X(004)    VALUE 'GU'.
       01  WRK-CHNG                      PIC  X(004)    VALUE 'CHNG'.
       01  WRK-ISRT                      PIC  X(004)    VALUE 'ISRT'.
       01  WRK-FUNCAO                    PIC  X(004)    VALUE SPACES.
       01  WRK-TELA                      PIC  X(008)    VALUE SPACES.
       01  WRK-NOVA-OPCAO                PIC  9(002)    VALUE ZEROS.
       01  WRK-OPCAO-ATUAL               PIC  9(002)    VALUE ZEROS.

       01  WRK-COD-USER-X.
           05 WRK-COD-USER               PIC  9(007)    VALUE ZEROS.

       01  WRK-COD-DEPTO.
           05 FILLER                     PIC  X(002)    VALUE SPACES.
           05 WRK-COD-DEPTO-N            PIC  9(004)    VALUE ZEROS.

       01  WRK-MENSAGEM.
           05 FILLER                     PIC  X(004)    VALUE SPACES.
           05 WRK-TRANSACAO              PIC  X(008)    VALUE SPACES.
           05 FILLER                     PIC  X(1988)   VALUE SPACES.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05 FILLER                     PIC  X(045)    VALUE
                '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                     PIC  X(012)    VALUE
                '- RET.CODE ='.
           05 WRK-RETURN-CODE            PIC  9(002)    VALUE ZEROS.
           05 FILLER                     PIC  X(011)    VALUE
                ' - LOCAL ='.
           05 WRK-LOCAL-ERRO             PIC  9(002)    VALUE ZEROS.
           05 FILLER                     PIC  X(002)    VALUE ' *'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA RUCAT797 - INPUT ***'.
      *----------------------------------------------------------------*

       01  WRK-INPUT.
           03 FILLER                   PIC  X(018)         VALUE SPACES.
           03 WRK-I-PFK                PIC  X(001)         VALUE SPACES.
           03 WRK-I-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-I-NOPCAO             PIC  X(002)         VALUE SPACES.
           03 WRK-I-PAGINA             PIC  9(002)         VALUE ZEROS.
           03 WRK-I-PROX-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-I-ANTE-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-I-CTERM              PIC  X(008)         VALUE SPACES.
           03 WRK-I-DADOS.
              05 WRK-I-CCONTR          PIC  9(009)         VALUE ZEROS.
              05 WRK-I-AGENCIA.
                 07 WRK-I-AGENCIA-N    PIC  9(005)         VALUE ZEROS.
              05 WRK-I-CONTA.
                 07 WRK-I-CONTA-N      PIC  9(08)          VALUE ZEROS.
              05 WRK-I-CNPJ-CPF        PIC  X(018)         VALUE SPACES.
              05 WRK-I-ITEM-LST        OCCURS 8.
                 07 WRK-I-REFBACEN     PIC  X(011)         VALUE SPACES.
                 07 WRK-I-CHVOPE.
                    10 WRK-I-CHVOPE-N PIC  9(009)          VALUE ZEROS.
                 07 WRK-I-SLDPRI.
                    10 WRK-I-SLDPRI-N PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-I-SLDENC.
                    10 WRK-I-SLDENC-N PIC  ZZZ.ZZZ.ZZ9,99  VALUE ZEROS.
                 07 WRK-I-CODBAS.
                    10 WRK-I-CODBAS-N PIC  9(02)           VALUE ZEROS.
                 07 WRK-I-SLDREN.
                    10 WRK-I-SLDREN-N PIC   Z.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-SLDTOT.
                 07 WRK-I-SLDTOT-N     PIC ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-I-MENSA           PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUCAT797  - OUTPUT  ***'.
      *----------------------------------------------------------------*

       01  WRK-OUTPUT.
           05 WRK-O-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05 WRK-O-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           03 WRK-O-SENHAS             PIC  X(037)         VALUE SPACES.
           03 WRK-O-NOPCAO             PIC  X(002)         VALUE SPACES.
           03 WRK-O-PAGINA             PIC  9(002)         VALUE ZEROS.
           03 WRK-O-PROX-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-O-ANTE-TELA          PIC  X(008)         VALUE SPACES.
           03 WRK-O-CTERM              PIC  X(008)         VALUE SPACES.
           03 WRK-O-DADOS.
              05 WRK-O-CCONTR          PIC  9(009)         VALUE ZEROS.
              05 WRK-O-AGENCIA.
                 07 WRK-O-AGENCIA-N    PIC  9(005)         VALUE ZEROS.
              05 WRK-O-CONTA.
                 07 WRK-O-CONTA-N      PIC  9(08)          VALUE ZEROS.
              05 WRK-O-CNPJ-CPF        PIC  X(018)         VALUE SPACES.
              05 WRK-O-ITEM-LST          OCCURS 8.
                 07 WRK-O-REFBACEN-ATTR  PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-REFBACEN       PIC  X(11)        VALUE SPACES.
                 07 WRK-O-CHVOPE-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-CHVOPE.
                    10 WRK-O-CHVOPE-N    PIC  9(09)        VALUE ZEROS.
                 07 WRK-O-SLDPRI-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-SLDPRI.
                    10 WRK-O-SLDPRI-N    PIC  ZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
                 07 WRK-O-SLDENC-ATTR    PIC  9(04) COMP   VALUE ZEROS.
                 07 WRK-O-SLDENC.
                    10 WRK-O-SLDENC-N  PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
                 07 WRK-O-CODBAS-ATTR  PIC  9(04) COMP     VALUE ZEROS.
                 07 WRK-O-CODBAS.
                    10 WRK-O-CODBAS-N  PIC  9(002)         VALUE ZEROS.
                 07 WRK-O-SLDREN-ATTR  PIC  9(04) COMP     VALUE ZEROS.
                 07 WRK-O-SLDREN.
                    10 WRK-O-SLDREN-N  PIC  Z.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-SLDTOT-ATTR     PIC  9(04) COMP     VALUE ZEROS.
              05 WRK-O-SLDTOT.
                 07 WRK-O-SLDTOT-N     PIC  ZZZ.ZZZ.ZZZ.ZZ9,99
                                                           VALUE ZEROS.
              05 WRK-O-MENSA           PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUCAT797 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUCAT797.
           03 WRK-660-LL                 PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-LL-OUTPUT          PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-SENHAS             PIC S9(004) COMP  VALUE 0037.
           03 WRK-660-NOVOOPC            PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-PAGINA             PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-PRTELA             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-ANTELA             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-CTERM              PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-CCONTR             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-AGENCIA            PIC S9(004) COMP  VALUE 0005.
           03 WRK-660-CONTA              PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-CNPJCPF            PIC S9(004) COMP  VALUE 0018.
           03 WRK-660-LISTA              OCCURS 8.
              05 WRK-660-REFBACEN        PIC S9(004) COMP  VALUE 0013.
              05 WRK-660-CHVOPE          PIC S9(004) COMP  VALUE 2011.
              05 WRK-660-SLDPRI          PIC S9(004) COMP  VALUE 2019.
              05 WRK-660-SLDENC          PIC S9(004) COMP  VALUE 2016.
              05 WRK-660-CODBAS          PIC S9(004) COMP  VALUE 2004.
              05 WRK-660-SLDREN          PIC S9(004) COMP  VALUE 2018.
           03 WRK-660-SLDTOT             PIC S9(004) COMP  VALUE 2020.
           03 WRK-660-MENSA              PIC S9(004) COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '* COPY I#RUEC00 - NAVEG-RURAL  *'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC00'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DE COMUNICACAO      ***'.
      *----------------------------------------------------------------*

       01  WRK-COMU-AREA.
           05  WRK-COMU-LL             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-ZZ             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-COMU-TRANCODE.
               10  WRK-COMU-TRANSACAO  PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  WRK-COMU-PFK        PIC  X(001)       VALUE SPACES.
           05  WRK-COMU-SENHAS         PIC  X(037)       VALUE SPACES.
           05  WRK-COMU-OPCAO          PIC  X(002)       VALUE SPACES.
           05  WRK-COMU-NROPER.
               10 WRK-COMU-NROPER-N    PIC  9(009)       VALUE ZEROS.
           05  WRK-COMU-AG.
               10 WRK-COMU-AG-N        PIC  9(005)       VALUE ZEROS.
           05  WRK-COMU-CC             PIC  X(09)        VALUE SPACES.
           05  WRK-COMU-CPF-CNPJ       PIC  X(17)        VALUE SPACES.


      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA DA BRAD7100         ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** AREA PARA DB2            ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '*** INCLUDE SQLCA            ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RUCAB003
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB002
           END-EXEC.

           EXEC SQL
               INCLUDE RUFIB045
           END-EXEC.

           EXEC SQL
              DECLARE CSR01-RUFIB045 SCROLL CURSOR FOR
              SELECT CREFT_RECOR
                   , VRENEG_CONTR_RURAL
                   , CBASE_LEGAL_RENEG
                   , VSDO_DVDOR_PRINC
                   , VTOT_ENCAR
                   , VTOT_JURO_MORA
                   , VTOT_MULTA
                FROM DB2PRD.TCONTR_RURAL_RENEG
               WHERE CCONTR = :RUFIB045.CCONTR
                 AND CREFT_RECOR > :RUFIB045.CREFT-RECOR
           END-EXEC.

       01  WRK-DESPREZA                 PIC S9(09)  COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       01 FILLER                        PIC  X(032)    VALUE
             '** RUCA7951 - FIM DA WORKING  **'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01 LNK-IO-PCB.
          05 LNK-IO-LTERM               PIC  X(008).
          05 FILLER                     PIC  X(002).
          05 LNK-IO-STATUS              PIC  X(002).
          05 FILLER                     PIC  X(012).
          05 LNK-IO-MODNAME             PIC  X(008).

       01 LNK-ALT-PCB.
          05 LNK-ALT-LTERM              PIC  X(008).
          05 FILLER                     PIC  X(002).
          05 LNK-ALT-STATUS             PIC  X(002).
          05 FILLER                     PIC  X(012).
          05 LNK-ALT-MODNAME            PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       0000-INICIAR                   SECTION.
      *----------------------------------------------------------------*

           ENTRY 'DLITCBL'            USING LNK-IO-PCB
                                            LNK-ALT-PCB.


           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       0000-10-POOL5000.
      *----------------------------------------------------------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF RETURN-CODE              EQUAL 04 OR 12
              PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO                          TO 0000-10-POOL5000.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'RUCAT797'             TO WRK-TELA.

           MOVE WRK-I-PAGINA           TO WRK-PAGINA

           IF WRK-TRANSACAO            NOT EQUAL 'RUCA7951'
              PERFORM 1200-RECEBER-CONTROLE
           ELSE
              PERFORM 3000-PROCESSAR-RUCAT797
           END-IF.

           IF WRK-TELA                 EQUAL 'RUCAT797'
              PERFORM 7000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1200-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-SENHAS        TO WRK-I-SENHAS
                                          WRK-O-SENHAS.
           MOVE 01                     TO WRK-I-PAGINA
                                          WRK-O-PAGINA.

           MOVE 'RUCA6630'             TO PROGRAMA-ANT.
           MOVE WRK-TRANSACAO          TO WRK-I-ANTE-TELA
                                          WRK-O-ANTE-TELA.

           MOVE WRK-COMU-NROPER-N      TO WRK-I-CCONTR
                                          WRK-O-CCONTR.

           MOVE WRK-COMU-AG-N          TO WRK-I-AGENCIA-N
                                          WRK-O-AGENCIA-N

           PERFORM 1220-OBTER-CC

           PERFORM 1230-OBTER-CPF-CNPJ

           PERFORM  1210-MONTAR-TELA.

           MOVE SPACES                 TO WRK-O-MENSA.


      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1210-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           PERFORM 1213-LIMPAR-LISTA.

           PERFORM 1300-CARREGAR-CONTRATOS.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1213-LIMPAR-LISTA               SECTION.
      *----------------------------------------------------------------*

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 8
              MOVE SPACES              TO WRK-O-REFBACEN(IND-1)
                                          WRK-O-CODBAS(IND-1)
                                          WRK-O-CHVOPE(IND-1)
                                          WRK-O-SLDPRI(IND-1)
                                          WRK-O-SLDENC(IND-1)
                                          WRK-O-SLDREN(IND-1)
              MOVE WRK-225             TO WRK-O-REFBACEN-ATTR(IND-1)
                                          WRK-O-CHVOPE-ATTR(IND-1)
                                          WRK-O-SLDPRI-ATTR(IND-1)
                                          WRK-O-SLDENC-ATTR(IND-1)
                                          WRK-O-CODBAS-ATTR(IND-1)
                                          WRK-O-SLDREN-ATTR(IND-1)
           END-PERFORM.

      *----------------------------------------------------------------*
       1213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1220-OBTER-CC                   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RUFIB002

           MOVE WRK-I-CCONTR           TO CCONTR
                                       OF RUFIB002
           EXEC SQL
              SELECT CCTA_CORR
                INTO :RUFIB002.CCTA-CORR
                FROM DB2PRD.CONTRATO_RURAL
               WHERE CCONTR = :RUFIB002.CCONTR
               FETCH FIRST ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUFIB002'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1220'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL ZEROS
              MOVE CCTA-CORR           OF RUFIB002
                                       TO WRK-S9-07
              MOVE WRK-9-07            TO WRK-I-CONTA-N
                                          WRK-O-CONTA-N
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1230-OBTER-CPF-CNPJ             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RUCAB003

           MOVE WRK-I-CCONTR           TO B003-CH-CONTR
                                       OF RUCAB003

           EXEC SQL
              SELECT B003_CGCPF1
                INTO :RUCAB003.B003-CGCPF1
                FROM DB2PRD.RUCA_CAD_ROOT
               WHERE B003_CH_CONTR = :RUCAB003.B003-CH-CONTR
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUCAB003'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'OPEN  '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1230'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE B003-CGCPF1            OF RUCAB003
                                       TO WRK-S9-15.
           MOVE WRK-9-15-X             TO WRK-CNPJ-CPF
           IF WRK-CNPJ-CPF-FILIAL      EQUAL ZEROS
              MOVE WRK-CNPJ-CPF-CGC    TO WRK-CPF-CGC
              MOVE WRK-CNPJ-CPF-CTRL   TO WRK-CPF-CTRL
              MOVE WRK-CPF             TO WRK-I-CNPJ-CPF
                                          WRK-O-CNPJ-CPF
           ELSE
              MOVE WRK-CNPJ-CPF-CGC    TO WRK-CPF-CGC
              MOVE WRK-CNPJ-CPF-FILIAL TO WRK-CNPJ-FILIAL
              MOVE WRK-CNPJ-CPF-CTRL   TO WRK-CPF-CTRL
              MOVE WRK-CNPJ            TO WRK-I-CNPJ-CPF
                                          WRK-O-CNPJ-CPF
           END-IF.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1300-CARREGAR-CONTRATOS         SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS                  TO WRK-VALOR-TOTAL
                                          WRK-QTD-CONTR

           PERFORM 1310-ABRIR-CURSOR.

           PERFORM 1320-LER-CURSOR.

           PERFORM                     UNTIL WRK-FIM-CSR
                                       EQUAL 'S'
                                       OR WRK-QTD-CONTR
                                       GREATER 8

              PERFORM 1340-MOVER-TABELA

              PERFORM 1320-LER-CURSOR
           END-PERFORM.

           PERFORM 1330-FECHAR-CURSOR.

           PERFORM 1350-OBTER-TOTAL-RENEG.

           MOVE WRK-169                TO WRK-O-SLDTOT-ATTR
           MOVE WRK-VALOR-TOTAL        TO WRK-O-SLDTOT-N.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1310-ABRIR-CURSOR               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RUFIB045.

           MOVE WRK-I-CCONTR           TO CCONTR
                                       OF RUFIB045

           MOVE LOW-VALUES             TO CREFT-RECOR
                                       OF RUFIB045

           EXEC SQL
              OPEN CSR01-RUFIB045
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUECB00C'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'OPEN  '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1205'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE 'N'                    TO WRK-FIM-CSR.

           MOVE WRK-I-PAGINA           TO WRK-PAGINA
           COMPUTE WRK-DESPREZA        = ((WRK-PAGINA - 1) * 8) + 1.

           MOVE 1                      TO WRK-DESPREZA.


      *----------------------------------------------------------------*
       1310-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1320-LER-CURSOR              SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              FETCH ABSOLUTE :WRK-DESPREZA
               FROM CSR01-RUFIB045
               INTO :RUFIB045.CREFT-RECOR
                  , :RUFIB045.VRENEG-CONTR-RURAL
                  , :RUFIB045.CBASE-LEGAL-RENEG
                  , :RUFIB045.VSDO-DVDOR-PRINC
                  , :RUFIB045.VTOT-ENCAR
                  , :RUFIB045.VTOT-JURO-MORA
                  , :RUFIB045.VTOT-MULTA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUFIB045'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'FETCH '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1300'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF  SQLCODE                 EQUAL +100
               MOVE 'S'                TO WRK-FIM-CSR
           ELSE
               ADD 1                   TO WRK-DESPREZA
           END-IF.

      *----------------------------------------------------------------*
       1320-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1330-FECHAR-CURSOR           SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              CLOSE CSR01-RUFIB045
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUFIB045'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'CLOSE '            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1305'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1330-99-FIM.                 EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1340-MOVER-TABELA            SECTION.
      *----------------------------------------------------------------*

           ADD 1                    TO WRK-QTD-CONTR.

           MOVE WRK-225             TO WRK-O-REFBACEN-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-CHVOPE-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-SLDPRI-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-SLDENC-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-CODBAS-ATTR
                                       (WRK-QTD-CONTR)
                                       WRK-O-SLDREN-ATTR
                                       (WRK-QTD-CONTR)

           INITIALIZE RUFIB002.
           MOVE CREFT-RECOR         OF RUFIB045
                                    TO WRK-O-REFBACEN
                                       (WRK-QTD-CONTR)
                                       WRK-I-REFBACEN
                                       (WRK-QTD-CONTR)
                                       CREFT-RECOR
                                    OF RUFIB002.

           MOVE 'N'                 TO WRK-INCONSIS

           PERFORM 3110-OBTER-CONTRATO-RUFI

           IF WRK-INCONSIS             EQUAL 'S'
              MOVE ZEROS               TO WRK-O-CHVOPE-N
                                          (WRK-QTD-CONTR)
           ELSE
              MOVE CCONTR              OF RUFIB002
                                       TO WRK-S9-09
              MOVE WRK-9-09            TO WRK-O-CHVOPE-N
                                          (WRK-QTD-CONTR)
           END-IF

           MOVE VRENEG-CONTR-RURAL     OF RUFIB045
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-O-SLDREN-N
                                         (WRK-QTD-CONTR).

           ADD VRENEG-CONTR-RURAL      OF RUFIB045
                                       TO WRK-VALOR-TOTAL.

           MOVE CBASE-LEGAL-RENEG      OF RUFIB045
                                       TO WRK-O-CODBAS
                                         (WRK-QTD-CONTR).

           MOVE VTOT-ENCAR             OF RUFIB045
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-O-SLDENC-N
                                          (WRK-QTD-CONTR).

           MOVE VTOT-JURO-MORA         OF RUFIB045
                                       TO WRK-S9-15V2.

           MOVE VTOT-MULTA             OF RUFIB045
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-O-SLDPRI-N
                                         (WRK-QTD-CONTR).

      *----------------------------------------------------------------*
       1340-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1350-OBTER-TOTAL-RENEG         SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              SELECT SUM(VRENEG_CONTR_RURAL)
                INTO :RUFIB045.VRENEG-CONTR-RURAL
                FROM DB2PRD.TCONTR_TEMPR_RENEG
               WHERE CCONTR = :RUFIB045.CCONTR
                 AND CREFT_RECOR > :RUFIB045.CREFT-RECOR
           END-EXEC.


           IF (SQLCODE                 NOT EQUAL ZEROS AND +100
                                                       AND -305)
           OR (SQLWARN                 EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUFIB045'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1350'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  NOT EQUAL ZEROS AND +100
               MOVE ZEROS              TO VRENEG-CONTR-RURAL
                                       OF RUFIB045
           END-IF.

           MOVE VRENEG-CONTR-RURAL     OF RUFIB045
                                       TO WRK-S9-15V2.
           MOVE WRK-9-15V2             TO WRK-VALOR-TOTAL.                            
      *----------------------------------------------------------------*
       1350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR-RUCAT797         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT.

           PERFORM 3050-EFETUAR-INSPECT.
           PERFORM 3200-DEVOLVER-TELA

           EVALUATE TRUE

           WHEN WRK-I-PFK EQUAL '.'
               CONTINUE

      * **     PF3
           WHEN WRK-I-PFK EQUAL 'C'
               PERFORM 3300-TRATAR-PF3

      * **     PF7
           WHEN WRK-I-PFK EQUAL '7'
               PERFORM 3400-PAGINA-ANTERIOR

           WHEN WRK-I-PFK EQUAL '8'
      * **     PF8
               PERFORM 3500-PROXIMA-PAGINA

      * **     PF INVALIDA
           WHEN OTHER
               MOVE 'PF INVALIDA'      TO WRK-O-MENSA
               PERFORM 1210-MONTAR-TELA
           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3050-EFETUAR-INSPECT            SECTION.
      *----------------------------------------------------------------*

           IF WRK-I-AGENCIA            NOT EQUAL SPACES AND LOW-VALUES
              INSPECT WRK-I-AGENCIA    REPLACING LEADING ' ' BY '0'
           END-IF.

           IF WRK-I-CONTA              NOT EQUAL SPACES AND LOW-VALUES
              INSPECT WRK-I-CONTA      REPLACING LEADING ' ' BY '0'
           END-IF.

           IF WRK-I-CNPJ-CPF           NOT EQUAL SPACES AND LOW-VALUES
              INSPECT WRK-I-CNPJ-CPF   REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3110-OBTER-CONTRATO-RUFI        SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
              SELECT CCONTR
                INTO :RUFIB002.CCONTR
                FROM DB2PRD.CONTRATO_RURAL
               WHERE CREFT_RECOR = :RUFIB002.CREFT-RECOR
               FETCH FIRST ROW ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W')
              MOVE 'DB2'               TO ERR-TIPO-ACESSO
              MOVE 'RUFIB002'          TO ERR-DBD-TAB
              MOVE SQLWARN             TO ERR-DBD-TAB(9:8)
              MOVE 'SELECT'            TO ERR-FUN-COMANDO
              MOVE SQLCODE             TO ERR-SQL-CODE
              MOVE '1400'              TO ERR-LOCAL
              MOVE SPACES              TO ERR-SEGM
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE 'S'                 TO WRK-INCONSIS
              GO TO 3110-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-SENHAS           TO WRK-O-SENHAS.
           MOVE WRK-I-PAGINA           TO WRK-O-PAGINA.
           MOVE WRK-I-PROX-TELA        TO WRK-O-PROX-TELA.
           MOVE WRK-I-ANTE-TELA        TO WRK-O-ANTE-TELA.
           MOVE WRK-I-CTERM            TO WRK-O-CTERM.
           MOVE WRK-I-CCONTR           TO WRK-O-CCONTR.
           MOVE WRK-I-AGENCIA          TO WRK-O-AGENCIA.
           MOVE WRK-I-CONTA            TO WRK-O-CONTA.
           MOVE WRK-I-CNPJ-CPF         TO WRK-O-CNPJ-CPF.

           PERFORM                     VARYING IND-1
                                       FROM 1
                                       BY 1
                                       UNTIL IND-1
                                       GREATER 8

              MOVE WRK-I-REFBACEN(IND-1)
                                       TO WRK-O-REFBACEN(IND-1)
              MOVE WRK-I-CHVOPE(IND-1) TO WRK-O-CHVOPE(IND-1)
              MOVE WRK-I-SLDPRI(IND-1) TO WRK-O-SLDPRI(IND-1)
              MOVE WRK-I-SLDENC(IND-1) TO WRK-O-SLDENC(IND-1)
              MOVE WRK-I-CODBAS(IND-1) TO WRK-O-CODBAS(IND-1)
              MOVE WRK-I-SLDREN(IND-1) TO WRK-O-SLDREN(IND-1)

              MOVE WRK-225             TO WRK-O-REFBACEN-ATTR(IND-1)
                                          WRK-O-CHVOPE-ATTR(IND-1)
                                          WRK-O-SLDPRI-ATTR(IND-1)
                                          WRK-O-SLDENC-ATTR(IND-1)
                                          WRK-O-CODBAS-ATTR(IND-1)
                                          WRK-O-SLDREN-ATTR(IND-1)
           END-PERFORM.

           MOVE WRK-225                TO WRK-O-SLDTOT-ATTR
           MOVE WRK-I-SLDTOT           TO WRK-O-SLDTOT.
           MOVE WRK-I-MENSA            TO WRK-O-MENSA.

      *----------------------------------------------------------------*
        3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3300-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE LENGTH OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE ZEROS                  TO WRK-COMU-ZZ.
           MOVE 'RUCA7951'             TO WRK-COMU-TRANSACAO.
           MOVE '.'                    TO WRK-COMU-PFK.
           MOVE WRK-I-SENHAS           TO WRK-COMU-SENHAS.
           MOVE ZEROS                  TO WRK-COMU-OPCAO.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

           MOVE 'RUCA6630'             TO WRK-TELA.


      *----------------------------------------------------------------*
        3300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3400-PAGINA-ANTERIOR            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-PAGINA           TO WRK-PAGINA
           IF WRK-I-PAGINA             GREATER 1
              COMPUTE WRK-PAGINA     = WRK-PAGINA - 1
              MOVE WRK-PAGINA          TO WRK-I-PAGINA
           END-IF.

           PERFORM 1213-LIMPAR-LISTA.

           MOVE WRK-I-AGENCIA-N        TO WRK-O-AGENCIA-N
           MOVE WRK-I-CCONTR           TO WRK-O-CCONTR
           PERFORM 1300-CARREGAR-CONTRATOS.

      *----------------------------------------------------------------*
       3400-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3500-PROXIMA-PAGINA             SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-PAGINA     = WRK-PAGINA + 1
           MOVE WRK-PAGINA          TO WRK-I-PAGINA

           PERFORM 1213-LIMPAR-LISTA.

           MOVE WRK-I-AGENCIA-N        TO WRK-O-AGENCIA-N
           MOVE WRK-I-CCONTR           TO WRK-O-CCONTR
           PERFORM 1300-CARREGAR-CONTRATOS.

      *----------------------------------------------------------------*
       3500-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       7000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT
                                       TO WRK-660-LL-OUTPUT
                                          WRK-O-LL
           MOVE LENGTH                 OF WRK-660-RUCAT797
                                       TO WRK-660-LL

           CALL 'BRAD0660'             USING WRK-OUTPUT
                                             WRK-660-RUCAT797.

           IF RETURN-CODE              NOT EQUAL ZEROS
              MOVE 'APL'               TO ERR-TIPO-ACESSO
              MOVE RETURN-CODE         TO WRK-RETURN-CODE
              MOVE 70                  TO WRK-LOCAL-ERRO
              MOVE WRK-ERRO-BRAD0660   TO ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT             TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       8000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-ROTINA-ERRO               SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO               NOT EQUAL SPACES AND LOW-VALUES
              MOVE 'RUCA7951'          TO ERR-PGM
           ELSE
              MOVE 'SENHAS02'          TO ERR-PGM
              MOVE 'RUCA7951'          TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER-X         TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.

           IF ERR-TIPO-ACESSO          EQUAL 'DB2'
              CALL 'BRAD7100'          USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
                                             SQLCA
           ELSE
              CALL 'BRAD7100'          USING LNK-IO-PCB
                                             ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
