      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     RUEC7950.
       AUTHOR.         RENATA AMORIM LEGARIO.
      *================================================================*
      *                   B S I   T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *     PROGRAMA....: RUEC7950                                     *
      *     PROGRAMADOR.: RENATA AMORIM LEGARIO                        *
      *     ANALISTA....: RENATA AMORIM LEGARIO                        *
      *     DATA........: 07/2017                                      *
      *----------------------------------------------------------------*
      *     OBJETIVO....: INCLUIR INFORMACOES SOBRE SEG.AGRICOLA       *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     RUECT795....: INCLUIR INFORMACOES SOBRE SEG.AGRICOLA       *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *================================================================*
EG1017*================================================================*
EG1017*                      A L T E R A C A O                         *
EG1017*================================================================*
EG1017*    ANALISTA....:   EVANDRO GUIMARAES       - BRQ               *
EG1017*    DATA........:   OUTUB/2017                                  *
EG1017*    OBJETIVO....:   EXIGIR SEGURO PARA OPERACOES COM PRONAF     *
EG1017*================================================================*
EG1017
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
       01  FILLER                      PIC  X(032)         VALUE
           '** RUEC7950 - AREA DE WORKING **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DE ATRIBUTOS        ***'.
      *----------------------------------------------------------------*

      *UNP,NUM,HIG,MOD
       01  WRK-49369                   PIC S9(008) COMP  VALUE +49369.
       01  FILLER                      REDEFINES WRK-49369.
           03 FILLER                   PIC  X(002).
           03 WRK-49369-X              PIC  X(002).

      *PRO,NUM,NOR,MOD
       01  WRK-241                     PIC S9(008) COMP  VALUE +241.
       01  FILLER                      REDEFINES WRK-241.
           03 FILLER                   PIC  X(002).
           03 WRK-241-X                PIC  X(002).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** VARIAVEIS AUXILIARES     ***'.
      *----------------------------------------------------------------*

       01  FILLER.
           03  WRK-SQLCODE-RUECB09J    PIC S9(009) COMP    VALUE ZEROS.
           03  WRK-MODULO              PIC  X(008)         VALUE SPACES.

       01  FILLER.
           03  WRK-INCONSIS            PIC  X(001)         VALUE SPACES.
EG0917     03  WRK-INC-ATVDD-FNALD     PIC  X(001)         VALUE SPACES.
           03  WRK-AUX-MENSA           PIC  X(079)         VALUE SPACES.
EG0917     03  WRK-X3.
EG0917         05  WRK-ZD3             PIC  9(003)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA POOL0496         ***'.
      *----------------------------------------------------------------*

       01  WRK-POOL0496-AREA.
           03  WRK-POOL0496-ENTRADA        PIC  X(14)  VALUE SPACES.
           03  WRK-POOL0496-SAIDA          PIC  X(13)  VALUE SPACES.
           03  FILLER REDEFINES WRK-POOL0496-SAIDA.
               05 FILLER                   PIC  X(02).
               05 WRK-POOL0496-SAIDA-R     PIC  9(09)V99.
           03  WRK-POOL0496-TAMANHO.
               05 WRK-POOL0496-TAM-ENT     PIC  9(02)  VALUE 14.
               05 WRK-POOL0496-TAM-SAI     PIC  9(02)  VALUE 13.
               05 WRK-POOL0496-TAM-INT     PIC  9(02)  VALUE 11.
               05 WRK-POOL0496-TAM-DEC     PIC  9(02)  VALUE 02.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DO MODULO RUEC8500  ***'.
      *----------------------------------------------------------------*

           COPY 'I#RUEC08'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA POOL5000         ***'.
      *----------------------------------------------------------------*

       01  WRK-GU                      PIC  X(004)         VALUE 'GU'.
       01  WRK-CHNG                    PIC  X(004)         VALUE 'CHNG'.
       01  WRK-ISRT                    PIC  X(004)         VALUE 'ISRT'.
       01  WRK-FUNCAO                  PIC  X(004)         VALUE SPACES.
       01  WRK-TELA                    PIC  X(008)         VALUE SPACES.
       01  WRK-NOVA-OPCAO              PIC  9(002)         VALUE ZEROS.
       01  WRK-OPCAO-ATUAL             PIC  9(002)         VALUE ZEROS.

       01  WRK-COD-USER-X.
           05 WRK-COD-USER             PIC  9(007)         VALUE ZEROS.

       01  WRK-COD-DEPTO.
           05 FILLER                   PIC  X(002)         VALUE SPACES.
           05 WRK-COD-DEPTO-N          PIC  9(004)         VALUE ZEROS.

       01  WRK-MENSAGEM.
           05 FILLER                   PIC  X(004)         VALUE SPACES.
           05 WRK-TRANSACAO            PIC  X(008)         VALUE SPACES.
           05 FILLER                   PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)       VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)       VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(011)       VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(002)       VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA RUECT795 - INPUT ***'.
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
           03 WRK-I-FUNCAO             PIC  X(009)         VALUE SPACES.
           03 WRK-I-CEDULA             PIC  9(009)         VALUE ZEROS.
           03 WRK-I-DADOS.
              05 WRK-I-SEGUR.
                 07 WRK-I-SEGUR-N      PIC  9(001)         VALUE ZEROS.
              05 WRK-I-CSEGU.
                 07 WRK-I-CSEGU-N      PIC  9(003)         VALUE ZEROS.
              05 WRK-I-NSEGU           PIC  X(040)         VALUE SPACES.
              05 WRK-I-NAPOL           PIC  X(030)         VALUE SPACES.
              05 WRK-I-FINAN.
                07 WRK-I-FINAN-N       PIC  9(001)         VALUE ZEROS.
              05 WRK-I-VFINAN.
                 07 WRK-I-VFINAN-N     PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              05 WRK-I-MENSA           PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUECT795  - OUTPUT  ***'.
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
           03 WRK-O-FUNCAO             PIC  X(009)         VALUE SPACES.
           03 WRK-O-CEDULA             PIC  9(009)         VALUE ZEROS.
           03 WRK-O-DADOS.
              05 WRK-O-SEGUR-ATTR      PIC  X(002)         VALUE SPACES.
              05 WRK-O-SEGUR.
                 07 WRK-O-SEGUR-N      PIC  9(001)         VALUE ZEROS.
              05 WRK-O-CSEGU-ATTR      PIC  X(002)         VALUE SPACES.
              05 WRK-O-CSEGU.
                 07 WRK-O-CSEGU-N      PIC  9(003)         VALUE ZEROS.
              05 WRK-O-NSEGU           PIC  X(040)         VALUE SPACES.
              05 WRK-O-NAPOL-ATTR      PIC  X(002)         VALUE SPACES.
              05 WRK-O-NAPOL           PIC  X(030)         VALUE ZEROS.
              05 WRK-O-FINAN-ATTR      PIC  X(002)         VALUE SPACES.
              05 WRK-O-FINAN.
                 07 WRK-O-FINAN-N      PIC  9(001)         VALUE ZEROS.
              05 WRK-O-VFINAN-ATTR     PIC  X(002)         VALUE SPACES.
              05 WRK-O-VFINAN.
                 07 WRK-O-VFINAN-N     PIC  ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.
              05 WRK-O-MENSA           PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA RUECT795 - BRAD0660 ***'.
      *----------------------------------------------------------------*

       01  WRK-660-RUECT795.
           03 WRK-660-LL                 PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-LL-OUTPUT          PIC S9(004) COMP  VALUE ZEROS.
           03 WRK-660-SENHAS             PIC S9(004) COMP  VALUE 0037.
           03 WRK-660-NOVOOPC            PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-PAGINA             PIC S9(004) COMP  VALUE 0002.
           03 WRK-660-PRTELA             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-ANTELA             PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-CTERM              PIC S9(004) COMP  VALUE 0008.
           03 WRK-660-FUNCAO             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-CEDULA             PIC S9(004) COMP  VALUE 0009.
           03 WRK-660-SEGUR              PIC S9(004) COMP  VALUE 2003.
           03 WRK-660-CSEGU              PIC S9(004) COMP  VALUE 0005.
           03 WRK-660-NSEGU              PIC S9(004) COMP  VALUE 2040.
           03 WRK-660-NAPOL              PIC S9(004) COMP  VALUE 2032.
           03 WRK-660-FINAN              PIC S9(004) COMP  VALUE 2003.
           03 WRK-660-VFINAN             PIC S9(004) COMP  VALUE 2016.
           03 WRK-660-MENSA              PIC S9(004) COMP  VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '* COPY I#RUEC00 - NAVEG-RURAL  *'.
      *----------------------------------------------------------------*

       COPY 'I#RUEC00'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
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
           05  WRK-COMU-OPCAO          PIC  9(002)       VALUE ZEROS.
           03  WRK-COMU-TERM           PIC  X(008)       VALUE SPACES.
           03  WRK-COMU-STATUS         PIC  X(002)       VALUE SPACES.
           03  WRK-COMU-MENSAGEM       PIC  X(079)       VALUE SPACES.
           03  WRK-COMU-PROX-TELA      PIC  X(008)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** COMUNICACAO COM RUEC7960 ***'.
      *----------------------------------------------------------------*

       01  WRK-7960-AREA.
           05  WRK-7960-LL             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-7960-ZZ             PIC S9(004) COMP  VALUE ZEROS.
           05  WRK-7960-TRANCODE.
               10  WRK-7960-TRANSACAO  PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  WRK-7960-PFK        PIC  X(001)       VALUE SPACES.
           05  WRK-7960-SENHAS         PIC  X(037)       VALUE SPACES.
           05  WRK-7960-OPCAO          PIC  9(002)       VALUE ZEROS.
           05  WRK-7960-SEGUR-SELCD    PIC  X(003)       VALUE SPACES.
           05  WRK-7960-SEGUR-SELCD-N REDEFINES
               WRK-7960-SEGUR-SELCD    PIC  9(003).
           05  WRK-7960-MENSAGEM       PIC  X(079)       VALUE SPACES.
           05  WRK-7960-ANTE-TELA      PIC  X(008)       VALUE SPACES.
           05  WRK-7960-NAVEGACAO      PIC  X(300)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INDICADORES DE NULIDADE  ***'.
      *----------------------------------------------------------------*

       01  WRK-RURCB006-NULL.
           05  WRK-CINDCD-EXIGE-SEG-N   PIC S9(04) COMP    VALUE ZEROS.

       01  WRK-RUECB09J-NULL.
           05  WRK-CSEGDR-N             PIC S9(04) COMP    VALUE ZEROS.
           05  WRK-VCOBER-APOLC-N       PIC S9(04) COMP    VALUE ZEROS.
           05  WRK-CAPOLC-SEGUR-N       PIC S9(04) COMP    VALUE ZEROS.
           05  WRK-HMANUT-REG-N         PIC S9(04) COMP    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA DA BRAD7100         ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA PARA DB2            ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE SQLCA            ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUECV007         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECV007
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUECB042         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB042
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RUECB09J         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RUECB09J
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RURCB006         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RURCB006
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RURCV035         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RURCV035
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RURCB004         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RURCB004
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** INCLUDE RURCB003         ***'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE RURCB003
           END-EXEC.

EG1017*----------------------------------------------------------------*
EG1017 01  FILLER                      PIC  X(032)         VALUE
EG1017     '*** INCLUDE RURCV000         ***'.
EG1017*----------------------------------------------------------------*
EG1017
EG1017     EXEC SQL
EG1017         INCLUDE RURCV000
EG1017     END-EXEC.
EG1017
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '** RUEC7950 - FIM DA WORKING  **'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.


           MOVE  WRK-GU                TO  WRK-FUNCAO.

      *-----------------*
       0000-10-POOL5000.
      *-----------------*

           CALL 'POOL5000'             USING WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM
                                             WRK-TELA
                                             WRK-NOVA-OPCAO
                                             WRK-COD-USER
                                             WRK-OPCAO-ATUAL
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 8000-FINALIZAR
           END-IF.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-10-POOL5000.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'RUECT795'            TO  WRK-TELA.

           IF WRK-TRANSACAO            NOT EQUAL  'RUEC7950'
              PERFORM 1200-RECEBER-CONTROLE
           ELSE
              PERFORM 3000-PROCESSAR-RUECT795
           END-IF.

           IF WRK-TELA              EQUAL  'RUECT795'
              PERFORM 7000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-ACESSAR-RUECB042           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CTERM            TO CTERM            OF RUECB042.

           EXEC SQL
             SELECT
               WNAVG_CREDT_RURAL
             INTO
               :RUECB042.WNAVG-CREDT-RURAL
             FROM  DB2PRD.NAVEGACAO_RURAL
             WHERE CTERM               =  :RUECB042.CTERM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB042'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '1100'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           MOVE WNAVG-CREDT-RURAL-TEXT OF RUECB042
                                       TO NAVEG-RURAL.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TRANSACAO           EQUAL 'RUEC7960'
               PERFORM 1300-TRATAR-RETORNO-PF9
               GO TO 1200-99-FIM
           END-IF.

           MOVE WRK-MENSAGEM           TO WRK-COMU-AREA.

           MOVE WRK-COMU-SENHAS        TO WRK-I-SENHAS
                                          WRK-O-SENHAS.
           MOVE WRK-COMU-TERM          TO WRK-I-CTERM
                                          WRK-O-CTERM.
           MOVE 01                     TO WRK-I-PAGINA
                                          WRK-O-PAGINA.

           PERFORM 1100-ACESSAR-RUECB042.

           MOVE 'RUEC7950'             TO PROGRAMA-ANT.
           PERFORM 1222-UPDATE-RUECB042.

           MOVE WRK-COMU-PROX-TELA     TO WRK-I-PROX-TELA
                                          WRK-O-PROX-TELA.
           MOVE WRK-TRANSACAO          TO WRK-I-ANTE-TELA
                                          WRK-O-ANTE-TELA.
           IF  WNAV-FUNCAO             OF NAVEGACAO-RURAL
                                       EQUAL 'A'
               MOVE 'ALTERACAO'        TO WRK-I-FUNCAO
                                          WRK-O-FUNCAO
           ELSE
               MOVE 'INCLUSAO'         TO WRK-I-FUNCAO
                                          WRK-O-FUNCAO
           END-IF.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO WRK-I-CEDULA
                                          WRK-O-CEDULA

           PERFORM 1210-LER-RUECB09J

           PERFORM 1215-MONTAR-TELA

           MOVE 'INFORME OS DADOS E TECLE ENTER'
                                       TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-LER-RUECB09J               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-RUECB09J-NULL.

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC      OF RUECB09J.
           MOVE WRK-I-CEDULA           TO CCDULA           OF RUECB09J.
           MOVE 1                      TO NAPOLC-SEGUR-OBRIG
                                                           OF RUECB09J.

           EXEC SQL
             SELECT
               NAPOLC_SEGUR_OBRIG
             , CSEGDR
             , VCOBER_APOLC_SEGUR
             , CAPOLC_SEGUR_RURAL
             , CSIT_REG_RURAL
             INTO
               :RUECB09J.NAPOLC-SEGUR-OBRIG
             , :RUECB09J.CSEGDR
                  :WRK-CSEGDR-N
             , :RUECB09J.VCOBER-APOLC-SEGUR
                  :WRK-VCOBER-APOLC-N
             , :RUECB09J.CAPOLC-SEGUR-RURAL
                  :WRK-CAPOLC-SEGUR-N
             , :RUECB09J.CSIT-REG-RURAL
             FROM  DB2PRD.TAPOLC_SEGUR_OBRIG
             WHERE CJUNC_DEPDC         = :RUECB09J.CJUNC-DEPDC
             AND   CCDULA              = :RUECB09J.CCDULA
             AND   NAPOLC_SEGUR_OBRIG >= :RUECB09J.NAPOLC-SEGUR-OBRIG
             FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'               )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09J'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '1210'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF WRK-CSEGDR-N             LESS ZEROS
              MOVE ZEROS               TO CSEGDR OF RUECB09J
           END-IF.

           IF WRK-VCOBER-APOLC-N       LESS ZEROS
              MOVE ZEROS               TO VCOBER-APOLC-SEGUR
                                                 OF RUECB09J
           END-IF.

           IF WRK-CAPOLC-SEGUR-N       LESS ZEROS
              MOVE SPACES              TO CAPOLC-SEGUR-RURAL
                                                 OF RUECB09J
           END-IF.

           MOVE SQLCODE                TO WRK-SQLCODE-RUECB09J.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1215-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*


           IF  WRK-SQLCODE-RUECB09J    EQUAL +100 OR
               CSIT-REG-RURAL OF RUECB09J
                                       NOT EQUAL 'A'
               MOVE SPACES             TO WRK-O-SEGUR
                                          WRK-O-CSEGU
                                          WRK-O-NSEGU
                                          WRK-O-NAPOL
                                          WRK-O-FINAN
                                          WRK-O-VFINAN
           ELSE
               MOVE 1                  TO WRK-O-SEGUR-N
               MOVE CSEGDR             OF RUECB09J
                                       TO WRK-O-CSEGU-N
                                          WRK-I-CSEGU-N
               PERFORM 4160-OBTER-SEGURADORA
               MOVE CAPOLC-SEGUR-RURAL OF RUECB09J
                                       TO WRK-O-NAPOL
               IF VCOBER-APOLC-SEGUR   OF RUECB09J NOT EQUAL ZEROS
                  MOVE 1               TO WRK-O-FINAN-N
                  MOVE VCOBER-APOLC-SEGUR
                                       OF RUECB09J
                                       TO WRK-O-VFINAN-N
               ELSE
                  IF CSEGDR            OF RUECB09J  EQUAL  522
                     MOVE 2            TO WRK-O-FINAN-N
                  ELSE
                     MOVE SPACES       TO WRK-O-FINAN
                  END-IF
                  MOVE SPACES          TO WRK-O-VFINAN
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1215-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1221-UPDATE-RUECV007            SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC      OF RUECV007.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO CCDULA           OF RUECV007.

           EXEC SQL
             UPDATE DB2PRD.V01CEDULA_TEMPR
             SET    CPROG              = :RUECV007.CPROG
             WHERE  CJUNC_DEPDC        = :RUECV007.CJUNC-DEPDC
             AND    CCDULA             = :RUECV007.CCDULA
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS )
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECV007'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '1221'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1222-UPDATE-RUECB042            SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CTERM            TO CTERM            OF RUECB042.

           EXEC SQL
             UPDATE DB2PRD.NAVEGACAO_RURAL
             SET WNAVG_CREDT_RURAL     = :RUECB042.WNAVG-CREDT-RURAL
             WHERE CTERM               = :RUECB042.CTERM
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100 )
           OR (SQLWARN0                EQUAL 'W'                )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB042'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '1222'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1222-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-TRATAR-RETORNO-PF9         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM (1:LENGTH OF  WRK-7960-AREA)
                                       TO  WRK-7960-AREA.

           IF  WRK-7960-SEGUR-SELCD    EQUAL ZEROS
                                          OR SPACES OR LOW-VALUES
               MOVE WRK-7960-MENSAGEM  TO  WRK-AUX-MENSA
               GO TO 1300-99-FIM
           END-IF.

           MOVE '.'                    TO  WRK-I-PFK
           MOVE WRK-7960-SEGUR-SELCD-N TO  WRK-I-CSEGU-N
                                           WRK-O-CSEGU-N.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2140-PASSAR-CONTROLE-RUEC7960   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-7960-AREA.
           MOVE LENGTH OF WRK-7960-AREA TO WRK-7960-LL
           MOVE ZEROS                  TO  WRK-7960-ZZ
           MOVE 'RUEC7950'             TO  WRK-7960-TRANSACAO
           MOVE '.'                    TO  WRK-7960-PFK
           MOVE WRK-I-SENHAS           TO  WRK-7960-SENHAS
           MOVE ZEROS                  TO  WRK-7960-OPCAO
                                           WRK-7960-SEGUR-SELCD
           MOVE SPACES                 TO  WRK-7960-MENSAGEM
           MOVE 'RUEC7950'             TO  WRK-7960-ANTE-TELA
           MOVE WRK-INPUT              TO  WRK-7960-NAVEGACAO.

           MOVE WRK-7960-AREA          TO  WRK-MENSAGEM.
           MOVE WRK-CHNG               TO  WRK-FUNCAO.
           MOVE 'RUEC7960'             TO  WRK-TELA.

      *----------------------------------------------------------------*
       2140-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-RUECT795         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  WRK-INPUT.
           PERFORM 3050-EFETUAR-INSPECT.
           PERFORM 1100-ACESSAR-RUECB042.
           PERFORM 3200-DEVOLVER-TELA

           EVALUATE TRUE

               WHEN WRK-I-PFK          EQUAL  '.'
                    PERFORM 3100-TRATAR-ENTER

      * **     PF3
               WHEN WRK-I-PFK          EQUAL  'C'
                    PERFORM 3300-TRATAR-PF3

      * **     PF09
               WHEN WRK-I-PFK          EQUAL  '9'
                    PERFORM 2140-PASSAR-CONTROLE-RUEC7960
      * **     PF11
               WHEN WRK-I-PFK          EQUAL  'B'
                    PERFORM 5100-TRATAR-PF11

      * **     PF INVALIDA
               WHEN OTHER
                    MOVE 'PF INVALIDA' TO WRK-O-MENSA

           END-EVALUATE.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3050-EFETUAR-INSPECT            SECTION.
      *----------------------------------------------------------------*

           IF  WRK-I-SEGUR             NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-I-SEGUR     REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-I-CSEGU             NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-I-CSEGU     REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-I-FINAN             NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-I-FINAN     REPLACING LEADING ' ' BY '0'
           END-IF.

           IF  WRK-I-VFINAN            NOT EQUAL SPACES AND LOW-VALUES
               INSPECT WRK-I-VFINAN    REPLACING LEADING ' ' BY '0'
           END-IF.

      *----------------------------------------------------------------*
       3050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3100-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA

           IF  WRK-INCONSIS            EQUAL 'S'
               GO TO 3100-99-FIM
           END-IF.


           MOVE 'TECLE PF11 PARA PROXIMA TELA'
                                       TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-SENHAS           TO WRK-O-SENHAS.
           MOVE WRK-I-PAGINA           TO WRK-O-PAGINA.
           MOVE WRK-I-PROX-TELA        TO WRK-O-PROX-TELA.
           MOVE WRK-I-ANTE-TELA        TO WRK-O-ANTE-TELA.
           MOVE WRK-I-CTERM            TO WRK-O-CTERM.
           MOVE WRK-I-FUNCAO           TO WRK-O-FUNCAO.
           MOVE WRK-I-CEDULA           TO WRK-O-CEDULA.
           MOVE WRK-I-SEGUR            TO WRK-O-SEGUR.
           MOVE WRK-I-CSEGU            TO WRK-O-CSEGU.
           MOVE WRK-I-NSEGU            TO WRK-O-NSEGU.
           MOVE WRK-I-NAPOL            TO WRK-O-NAPOL.
           MOVE WRK-I-FINAN            TO WRK-O-FINAN.
           MOVE WRK-I-VFINAN           TO WRK-O-VFINAN.
           MOVE WRK-I-MENSA            TO WRK-O-MENSA.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-TRATAR-PF3                 SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE ZEROS                  TO WRK-COMU-ZZ.
           MOVE 'RUEC7950'             TO WRK-COMU-TRANSACAO.
           MOVE '.'                    TO WRK-COMU-PFK.
           MOVE WRK-I-SENHAS           TO WRK-COMU-SENHAS.
           MOVE ZEROS                  TO WRK-COMU-OPCAO.
           MOVE WRK-I-CTERM            TO WRK-COMU-TERM
           MOVE SPACES                 TO WRK-COMU-STATUS
                                          WRK-COMU-MENSAGEM.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

           IF  WNAV-FUNCAO             OF NAVEGACAO-RURAL
                                       EQUAL 'A'
               MOVE 'RUEC8710'         TO WRK-TELA
           ELSE
               MOVE 'RUEC7000'         TO WRK-TELA
           END-IF.

           MOVE 'RUEC7030'             TO CPROG            OF RUECV007.
           PERFORM 1221-UPDATE-RUECV007.

      *----------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-CONSISTIR-AREA-ENTRADA     SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-INCONSIS.

           PERFORM 4100-CONSISTIR-CAMPOS.

           IF  WRK-INCONSIS            EQUAL 'S'
               GO TO 4000-99-FIM
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-CONSISTIR-CAMPOS           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-O-MENSA
                                          WRK-INCONSIS
                                          WRK-O-SEGUR-ATTR
                                          WRK-O-CSEGU-ATTR
                                          WRK-O-NAPOL-ATTR
                                          WRK-O-FINAN-ATTR
                                          WRK-O-VFINAN-ATTR

           IF WRK-I-SEGUR              IS NUMERIC
              IF WRK-I-SEGUR-N         NOT EQUAL 1 AND 2
                 MOVE 'S'              TO WRK-INCONSIS
                 MOVE 'CONTEUDO INVALIDO'
                                       TO WRK-AUX-MENSA
                 PERFORM 4400-MOVER-MENSAGEM
                 MOVE WRK-49369-X      TO WRK-O-SEGUR-ATTR
                 GO TO 4100-99-FIM
              END-IF
           ELSE
             IF WRK-I-SEGUR            NOT EQUAL SPACES
                MOVE 'S'               TO WRK-INCONSIS
                MOVE 'CONTEUDO INVALIDO'
                                       TO WRK-AUX-MENSA
                PERFORM 4400-MOVER-MENSAGEM
                MOVE WRK-49369-X       TO WRK-O-SEGUR-ATTR
                GO TO 4100-99-FIM
             END-IF
           END-IF.

EG1017     PERFORM 4150-OBTER-PROGRAMA-BACEN
           MOVE SPACES                 TO CATVDD-BACEN OF RURCB004
RE1017     MOVE COD-ATIVIDADE          TO CATVDD-RURAL OF RURCB004.
RE1017
RE1017     EXEC SQL
RE1017       SELECT CATVDD_BACEN
RE1017         INTO :RURCB004.CATVDD-BACEN
RE1017       FROM   DB2PRD.ATIVIDADE_RURAL
RE1017       WHERE  CATVDD_RURAL       = :RURCB004.CATVDD-RURAL
RE1017     END-EXEC.
RE1017
RE1017     IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
RE1017     OR (SQLWARN0                EQUAL 'W'       )
RE1017         MOVE 'DB2'              TO ERR-TIPO-ACESSO
RE1017         MOVE 'RURCB004'         TO ERR-DBD-TAB
RE1017         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
RE1017         MOVE 'SELECT'           TO ERR-FUN-COMANDO
RE1017         MOVE SQLCODE            TO ERR-SQL-CODE
RE1017         MOVE '41OA'             TO ERR-LOCAL
RE1017         MOVE SPACES             TO ERR-SEGM
RE1017         PERFORM 9999-ROTINA-ERRO
RE1017     END-IF.
RE1017
RE1017     IF  SQLCODE                 EQUAL +100
RE1017         MOVE 'S'                TO WRK-INCONSIS
RE1017         MOVE COD-ATIVIDADE      TO WRK-ZD3
RE1017         STRING 'ATIVIDADE '
RE1017                WRK-X3
RE1017                ' NAO CADASTRADA' DELIMITED BY SIZE
RE1017                                  INTO WRK-AUX-MENSA
RE1017        PERFORM 4400-MOVER-MENSAGEM
RE1017        GO TO 4100-99-FIM
RE1017     END-IF.
RE1017
           MOVE SPACES                 TO CFNALD-RECOR OF RURCB003
RE1017     MOVE COD-FINALIDADE         TO CFNALD-RURAL OF RURCB003.
RE1017
RE1017     EXEC SQL
RE1017       SELECT CFNALD_RECOR
RE1017         INTO :RURCB003.CFNALD-RECOR
RE1017       FROM   DB2PRD.FNALD_RURAL
RE1017       WHERE  CFNALD_RURAL       = :RURCB003.CFNALD-RURAL
RE1017     END-EXEC.
RE1017
RE1017     IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
RE1017     OR (SQLWARN0                EQUAL 'W'       )
RE1017         MOVE 'DB2'              TO ERR-TIPO-ACESSO
RE1017         MOVE 'RURCB003'         TO ERR-DBD-TAB
RE1017         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
RE1017         MOVE 'SELECT'           TO ERR-FUN-COMANDO
RE1017         MOVE SQLCODE            TO ERR-SQL-CODE
RE1017         MOVE '410B'             TO ERR-LOCAL
RE1017         MOVE SPACES             TO ERR-SEGM
RE1017         PERFORM 9999-ROTINA-ERRO
RE1017     END-IF.
RE1017
RE1017     IF  SQLCODE                 EQUAL +100
RE1017         MOVE 'S'                TO WRK-INCONSIS
RE1017         MOVE COD-FINALIDADE     TO WRK-ZD3
RE1017         STRING 'FINALIDADE '
RE1017                WRK-X3
RE1017                ' NAO CADASTRADA' DELIMITED BY SIZE
RE1017                                  INTO WRK-AUX-MENSA
RE1017        PERFORM 4400-MOVER-MENSAGEM
RE1017        GO TO 4100-99-FIM
RE1017     END-IF.

RE1017***  NAO EXIGE SEGURO PARA AGRO INDUSTRIA
RE1017***  EXIGE SEGURO PARA PRONAF CUSTEIO AGRICOLA COM ZONEAMENTO
RE1017
RE1017     IF (( COD-ORIG-REC          EQUAL 035
RE1017                                    OR 037 )
RE1017     AND   COD-FINALIDADE        EQUAL 027 )
RE1017         NEXT SENTENCE
RE1017     ELSE
RE1017       IF WRK-I-SEGUR-N        NOT EQUAL 1
RE1017        IF CPROG-RURAL  OF RURCV000  EQUAL '0001'  AND
RE1017           CATVDD-BACEN OF RURCB004  EQUAL '00001' AND
RE1017           CFNALD-RECOR OF RURCB003  EQUAL '00002' AND
RE1017           COD-ZONEAMENTO            EQUAL '1'
RE1017           MOVE 'S'                 TO WRK-INCONSIS
RE1017           MOVE 'SEGURO E OBRIGATORIO PARA OPERACAO COM PRONAF AGR
RE1017-               'ICOLA E ZONEAMTO'    TO WRK-AUX-MENSA
RE1017           PERFORM 4400-MOVER-MENSAGEM
RE1017           MOVE WRK-49369-X         TO WRK-O-SEGUR-ATTR
RE1017           GO TO 4100-99-FIM
RE1017        END-IF
RE1017       END-IF
RE1017     END-IF.

           IF  WRK-I-SEGUR-N           NOT EQUAL 1
               MOVE SPACES             TO WRK-O-CSEGU
                                          WRK-O-NSEGU
                                          WRK-O-NAPOL
                                          WRK-O-FINAN
                                          WRK-O-VFINAN
               MOVE WRK-241-X          TO WRK-O-CSEGU-ATTR
                                          WRK-O-NAPOL-ATTR
                                          WRK-O-FINAN-ATTR
                                          WRK-O-VFINAN-ATTR
               GO TO 4100-99-FIM
           END-IF.

           IF  WRK-I-CSEGU             NOT NUMERIC OR
               WRK-I-CSEGU-N           EQUAL ZEROS
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'INFORMAR SEGURADORA'
                                       TO WRK-AUX-MENSA
               PERFORM 4400-MOVER-MENSAGEM
               MOVE WRK-49369-X        TO WRK-O-CSEGU-ATTR
               GO TO 4100-99-FIM
           END-IF.

           PERFORM 4160-OBTER-SEGURADORA

           IF  WRK-I-NSEGU             EQUAL SPACES
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'COD.SEGURADORA NAO ENCONTRADO'
                                       TO WRK-AUX-MENSA
               PERFORM 4400-MOVER-MENSAGEM
               MOVE WRK-49369-X        TO WRK-O-CSEGU-ATTR
               GO TO 4100-99-FIM
           END-IF.

           IF  CTPO-GARTD OF RURCV035 NOT EQUAL 3
               MOVE 'S'                TO WRK-INCONSIS
               MOVE SPACES             TO WRK-O-NSEGU
               MOVE 'COD.NAO E SEGURADORA'
                                       TO WRK-AUX-MENSA
               PERFORM 4400-MOVER-MENSAGEM
               MOVE WRK-49369-X         TO WRK-O-CSEGU-ATTR
               GO TO 4100-99-FIM
           END-IF.

           IF  WRK-I-NAPOL             EQUAL SPACES OR LOW-VALUES
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'INFORMAR NUMERO DA APOLICE/PROPOSTA'
                                       TO WRK-AUX-MENSA
               PERFORM 4400-MOVER-MENSAGEM
               MOVE WRK-49369-X        TO WRK-O-NAPOL-ATTR
               GO TO 4100-99-FIM
           END-IF.

           IF WRK-I-CSEGU-N            NOT EQUAL 522
              MOVE SPACES              TO WRK-O-FINAN
                                          WRK-O-VFINAN
              MOVE WRK-241-X           TO WRK-O-FINAN-ATTR
                                          WRK-O-VFINAN-ATTR
              GO TO 4100-99-FIM
           END-IF.

           IF (WRK-I-FINAN             NOT NUMERIC) OR
              (WRK-I-FINAN-N           NOT EQUAL 1 AND 2)
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'CONTEUDO INVALIDO'
                                       TO WRK-AUX-MENSA
               PERFORM 4400-MOVER-MENSAGEM
               MOVE WRK-49369-X        TO WRK-O-FINAN-ATTR
               GO TO 4100-99-FIM
           END-IF.

           IF  WRK-I-FINAN-N           EQUAL 2
               MOVE SPACES             TO WRK-O-VFINAN
               MOVE WRK-241-X          TO WRK-O-VFINAN-ATTR
               GO TO 4100-99-FIM
           END-IF.

           MOVE WRK-I-VFINAN           TO WRK-POOL0496-ENTRADA
           MOVE SPACES                 TO WRK-POOL0496-SAIDA

           CALL 'POOL0496'             USING WRK-POOL0496-ENTRADA
                                             WRK-POOL0496-SAIDA
                                             WRK-POOL0496-TAMANHO

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'S'                TO WRK-INCONSIS
               MOVE 'VALOR PREMIO DO SEGURO INVALIDO'
                                       TO WRK-AUX-MENSA
               PERFORM 4400-MOVER-MENSAGEM
               MOVE WRK-49369-X        TO WRK-O-VFINAN-ATTR
               GO TO 4100-99-FIM
           ELSE
               MOVE WRK-POOL0496-SAIDA-R
                                       TO WRK-O-VFINAN-N
           END-IF.


      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

EG1017*----------------------------------------------------------------*
EG1017 4150-OBTER-PROGRAMA-BACEN       SECTION.
EG1017*----------------------------------------------------------------*
EG1017     MOVE SPACES                 TO CPROG-RURAL OF RURCV000
EG1017***  EXIGE SEGURO PARA OPERACOES COM PRONAF

EG1017     IF  COD-ORIG-REC            NOT NUMERIC
EG1017     OR  COD-ORIG-REC            EQUAL ZEROS
EG1017         GO TO 4150-99-FIM
EG1017     END-IF.
EG1017
EG1017     MOVE  COD-ORIG-REC          TO CORIGE-REC OF RURCV000.
EG1017
EG1017     EXEC SQL
EG1017       SELECT  CPROG_RURAL
EG1017       INTO   :RURCV000.CPROG-RURAL
EG1017       FROM    DB2PRD.V01ORIGEM_RECURSO
EG1017       WHERE   CORIGE_REC         = :RURCV000.CORIGE-REC
EG1017     END-EXEC.
EG1017
EG1017     IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
EG1017     OR (SQLWARN0                EQUAL 'W'       )
EG1017         MOVE 'DB2'              TO ERR-TIPO-ACESSO
EG1017         MOVE 'RURCV000'         TO ERR-DBD-TAB
EG1017         MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
EG1017         MOVE 'SELECT'           TO ERR-FUN-COMANDO
EG1017         MOVE SQLCODE            TO ERR-SQL-CODE
EG1017         MOVE '4150'             TO ERR-LOCAL
EG1017         MOVE SPACES             TO ERR-SEGM
EG1017         PERFORM 9999-ROTINA-ERRO
EG1017     END-IF.
EG1017
EG1017     IF  SQLCODE                 EQUAL +100
EG1017         MOVE SPACES             TO CPROG-RURAL OF RURCV000
EG1017     END-IF.
EG1017
EG1017*----------------------------------------------------------------*
EG1017 4150-99-FIM.                    EXIT.
EG1017*----------------------------------------------------------------*
EG1017
      *----------------------------------------------------------------*
       4160-OBTER-SEGURADORA           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-I-CSEGU-N          TO CGARTD OF RURCV035

           EXEC SQL
             SELECT IPSSOA,
                    CTPO_GARTD
               INTO
                 :RURCV035.IPSSOA,
                 :RURCV035.CTPO-GARTD
             FROM  DB2PRD.V01GARTD_RURAL
             WHERE CGARTD              = :RURCV035.CGARTD
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100)
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RURCV035'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '4160'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL +100
              MOVE SPACES              TO WRK-I-NSEGU
                                          WRK-O-NSEGU
           ELSE
              MOVE IPSSOA OF RURCV035  TO WRK-I-NSEGU
                                          WRK-O-NSEGU
           END-IF.

      *----------------------------------------------------------------*
       4160-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       4300-PASSAR-CONTROLE            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  WRK-COMU-AREA.

           MOVE SPACES                 TO WRK-TELA
                                          WRK-O-MENSA
                                          LK8-CODIGO-PROXPGM
                                          LK8-AREA-NAVEGA

           MOVE WRK-CHNG               TO WRK-FUNCAO.
           MOVE LENGTH                 OF WRK-COMU-AREA
                                       TO WRK-COMU-LL.
           MOVE 'RUEC7950'             TO WRK-COMU-TRANSACAO.
           MOVE '.'                    TO WRK-COMU-PFK.
           MOVE WRK-I-SENHAS           TO WRK-COMU-SENHAS.
           MOVE ZEROS                  TO WRK-COMU-OPCAO.
           MOVE WRK-I-CTERM            TO WRK-COMU-TERM
           MOVE SPACES                 TO WRK-COMU-STATUS
                                          WRK-COMU-MENSAGEM.
           MOVE WRK-COMU-AREA          TO WRK-MENSAGEM.

           PERFORM 4310-ACESSAR-RUEC8500

           MOVE 'RUEC7951'             TO WRK-TELA

           IF  WNAV-FUNCAO             OF NAVEGACAO-RURAL
                                       NOT EQUAL 'A'
               MOVE 'RUEC7950'         TO CPROG   OF RUECV007
               PERFORM 1221-UPDATE-RUECV007
           END-IF.

      *----------------------------------------------------------------*
       4300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       4310-ACESSAR-RUEC8500           SECTION.
      *---------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO LK8-CODIGO-AGENCIA.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO LK8-CODIGO-CEDULA.
           MOVE WRK-I-CTERM            TO LK8-CODIGO-TERMINAL.

           MOVE 07                     TO LK8-NUMORD-CHAMADOR.
           MOVE 'RUEC8500'             TO WRK-MODULO.
           CALL WRK-MODULO          USING LNK-IO-PCB
                                          LNK-ALT-PCB
                                          LK8-AREA-NAVEGA.

           IF  LK8-STATUS-RETORNO      EQUAL SPACES
               GO TO 4310-99-FIM
           END-IF.

           EVALUATE LK8-STATUS-RETORNO
             WHEN   'ER'
               MOVE LK8-MENSAGEM       TO WRK-O-MENSA
             WHEN   'BL'
               MOVE LK8-MENSAGEM       TO WRK-O-MENSA
             WHEN   'DB'
               MOVE LK8-AREA-ENTRADA   TO ERR-TEXTO (01:24)
               MOVE LK8-STATUS-RETORNO TO ERR-TEXTO (25:02)
               MOVE LK8-CODIGO-PROXPGM TO ERR-TEXTO (27:08)
               MOVE LK8-MENSAGEM       TO ERR-TEXTO (35:31)
               MOVE 'RUEC8500'         TO ERR-MODULO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-EVALUATE.

      *---------------------------------------------------------------*
       4310-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *----------------------------------------------------------------*
       4400-MOVER-MENSAGEM             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-O-MENSA             EQUAL SPACES
               MOVE WRK-AUX-MENSA      TO WRK-O-MENSA
           END-IF.

      *----------------------------------------------------------------*
       4400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       5100-TRATAR-PF11                SECTION.
      *----------------------------------------------------------------*

           PERFORM 4000-CONSISTIR-AREA-ENTRADA.

           IF  WRK-INCONSIS            EQUAL 'S'
               GO TO 5100-99-FIM
           END-IF.

           IF  WNAV-FUNCAO             OF NAVEGACAO-RURAL
                                       EQUAL 'A'
               PERFORM 5300-TRATAR-ALTERACAO
           ELSE
               IF WRK-I-SEGUR-N        EQUAL 1
                  PERFORM 5200-INSERT-RUECB09J
               END-IF
           END-IF.

           PERFORM 4300-PASSAR-CONTROLE.

      *----------------------------------------------------------------*
       5100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5200-INSERT-RUECB09J            SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC       OF RUECB09J.
           MOVE CCDULA                 OF NAVEGACAO-RURAL
                                       TO CCDULA            OF RUECB09J.
           MOVE 1                      TO NAPOLC-SEGUR-OBRIG
                                                            OF RUECB09J.
           MOVE 1                      TO CTPO-SEGUR-CONTT  OF RUECB09J.
           MOVE WRK-I-CSEGU-N          TO CSEGDR            OF RUECB09J.
           MOVE ZEROS                  TO PALIQT-SEGUR-BACEN
                                                            OF RUECB09J.
           MOVE WRK-I-NAPOL(1:20)      TO CAPOLC-SEGUR-RURAL
                                                            OF RUECB09J.
           MOVE '01.01.0001'           TO DINIC-VGCIA-SEGUR OF RUECB09J.
           MOVE '01.01.0001'           TO DFIM-VGCIA-SEGUR  OF RUECB09J.

           MOVE WRK-COD-USER-X         TO CUSUAR-INCL       OF RUECB09J.

           MOVE SPACES                 TO CUSUAR-MANUT      OF RUECB09J.
           MOVE SPACES                 TO HMANUT-REG        OF RUECB09J.
           MOVE -1                     TO WRK-HMANUT-REG-N
           MOVE 'A'                    TO CSIT-REG-RURAL    OF RUECB09J.

           IF WRK-I-VFINAN-N           EQUAL ZEROS
              MOVE ZEROS               TO VCOBER-APOLC-SEGUR
                                                            OF RUECB09J
           ELSE
              MOVE WRK-I-VFINAN        TO WRK-POOL0496-ENTRADA
              MOVE SPACES              TO WRK-POOL0496-SAIDA

              CALL 'POOL0496'          USING WRK-POOL0496-ENTRADA
                                             WRK-POOL0496-SAIDA
                                             WRK-POOL0496-TAMANHO

              MOVE WRK-POOL0496-SAIDA-R TO VCOBER-APOLC-SEGUR
                                                            OF RUECB09J
           END-IF.

           EXEC SQL
             INSERT INTO DB2PRD.TAPOLC_SEGUR_OBRIG
               ( CJUNC_DEPDC
               , CCDULA
               , NAPOLC_SEGUR_OBRIG
               , CTPO_SEGUR_CONTT
               , CSEGDR
               , PALIQT_SEGUR_BACEN
               , VCOBER_APOLC_SEGUR
               , CAPOLC_SEGUR_RURAL
               , DINIC_VGCIA_SEGUR
               , DFIM_VGCIA_SEGUR
               , CSIT_REG_RURAL
               , CUSUAR_INCL
               , HINCL_REG
               , CUSUAR_MANUT
               , HMANUT_REG)
             VALUES
               ( :RUECB09J.CJUNC-DEPDC
               , :RUECB09J.CCDULA
               , :RUECB09J.NAPOLC-SEGUR-OBRIG
               , :RUECB09J.CTPO-SEGUR-CONTT
               , :RUECB09J.CSEGDR
               , :RUECB09J.PALIQT-SEGUR-BACEN
               , :RUECB09J.VCOBER-APOLC-SEGUR
               , :RUECB09J.CAPOLC-SEGUR-RURAL
               , :RUECB09J.DINIC-VGCIA-SEGUR
               , :RUECB09J.DFIM-VGCIA-SEGUR
               , :RUECB09J.CSIT-REG-RURAL
               , :RUECB09J.CUSUAR-INCL
               , CURRENT TIMESTAMP
               , :RUECB09J.CUSUAR-MANUT
               , :RUECB09J.HMANUT-REG
                   :WRK-HMANUT-REG-N)
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND -803)
           OR (SQLWARN0                EQUAL 'W'       )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09J'         TO ERR-DBD-TAB
               MOVE 'INSERT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '5200'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE                  EQUAL -803
              PERFORM 5350-UPDATE-RUECB09J
           END-IF.

      *----------------------------------------------------------------*
       5200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5300-TRATAR-ALTERACAO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-LER-RUECB09J

           IF WRK-SQLCODE-RUECB09J EQUAL ZEROS
              IF WRK-I-SEGUR-N     NOT EQUAL 1
                 PERFORM 5351-UPDATE-RUECB09J
              ELSE
                 PERFORM 5350-UPDATE-RUECB09J
              END-IF
              GO TO 5300-99-FIM
           END-IF.

           IF WRK-I-SEGUR-N        EQUAL 1
              PERFORM 5200-INSERT-RUECB09J
           END-IF.

      *----------------------------------------------------------------*
       5300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5350-UPDATE-RUECB09J            SECTION.
      *----------------------------------------------------------------*

           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC       OF RUECB09J
           MOVE WRK-I-CEDULA           TO CCDULA            OF RUECB09J

           MOVE WRK-I-CSEGU-N          TO CSEGDR            OF RUECB09J.
           MOVE WRK-I-NAPOL(1:20)      TO CAPOLC-SEGUR-RURAL
                                                            OF RUECB09J.

           MOVE WRK-COD-USER-X         TO CUSUAR-MANUT      OF RUECB09J.

           IF WRK-I-FINAN-N            NOT EQUAL 1
              MOVE ZEROS               TO  WRK-I-VFINAN-N
           END-IF.

           IF WRK-I-VFINAN-N           EQUAL ZEROS
              MOVE ZEROS               TO VCOBER-APOLC-SEGUR
                                                            OF RUECB09J
           ELSE
              MOVE WRK-I-VFINAN        TO WRK-POOL0496-ENTRADA
              MOVE SPACES              TO WRK-POOL0496-SAIDA

              CALL 'POOL0496'          USING WRK-POOL0496-ENTRADA
                                             WRK-POOL0496-SAIDA
                                             WRK-POOL0496-TAMANHO

              MOVE WRK-POOL0496-SAIDA-R TO VCOBER-APOLC-SEGUR
                                                            OF RUECB09J
           END-IF.
           MOVE 'A'                    TO CSIT-REG-RURAL    OF RUECB09J.

           EXEC SQL
             UPDATE DB2PRD.TAPOLC_SEGUR_OBRIG
             SET CSEGDR                = :RUECB09J.CSEGDR
               , CAPOLC_SEGUR_RURAL    = :RUECB09J.CAPOLC-SEGUR-RURAL
               , VCOBER_APOLC_SEGUR    = :RUECB09J.VCOBER-APOLC-SEGUR
               , CSIT_REG_RURAL        = :RUECB09J.CSIT-REG-RURAL
               , CUSUAR_MANUT          = :RUECB09J.CUSUAR-MANUT
               , HMANUT_REG            = CURRENT TIMESTAMP
             WHERE CJUNC_DEPDC         = :RUECB09J.CJUNC-DEPDC
             AND   CCDULA              = :RUECB09J.CCDULA
             AND   NAPOLC_SEGUR_OBRIG  = :RUECB09J.NAPOLC-SEGUR-OBRIG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS   )
           OR (SQLWARN0                EQUAL 'W'         )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09J'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '5350'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5350-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       5351-UPDATE-RUECB09J            SECTION.
      *----------------------------------------------------------------*
           MOVE AGENCIA                OF NAVEGACAO-RURAL
                                       TO CJUNC-DEPDC       OF RUECB09J
           MOVE WRK-I-CEDULA           TO CCDULA            OF RUECB09J
           MOVE WRK-COD-USER-X         TO CUSUAR-MANUT      OF RUECB09J.
           MOVE 'I'                    TO CSIT-REG-RURAL    OF RUECB09J.

           EXEC SQL
             UPDATE DB2PRD.TAPOLC_SEGUR_OBRIG
             SET CSIT_REG_RURAL        = :RUECB09J.CSIT-REG-RURAL
               , CUSUAR_MANUT          = :RUECB09J.CUSUAR-MANUT
               , HMANUT_REG            = CURRENT TIMESTAMP
             WHERE CJUNC_DEPDC         = :RUECB09J.CJUNC-DEPDC
             AND   CCDULA              = :RUECB09J.CCDULA
             AND   NAPOLC_SEGUR_OBRIG  = :RUECB09J.NAPOLC-SEGUR-OBRIG
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS   )
           OR (SQLWARN0                EQUAL 'W'         )
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               MOVE 'RUECB09J'         TO ERR-DBD-TAB
               MOVE SQLWARN            TO ERR-DBD-TAB (9:8)
               MOVE 'UPDATE'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '5351'             TO ERR-LOCAL
               MOVE SPACES             TO ERR-SEGM
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       5351-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       7000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT   TO WRK-660-LL-OUTPUT
                                          WRK-O-LL
           MOVE LENGTH OF WRK-660-RUECT795
                                       TO WRK-660-LL

           CALL  'BRAD0660'            USING WRK-OUTPUT
                                             WRK-660-RUECT795.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO WRK-RETURN-CODE
              MOVE  70                 TO WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO WRK-FUNCAO.
           MOVE  WRK-OUTPUT            TO WRK-MENSAGEM.

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
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO               NOT EQUAL  SPACES  AND
                                                  LOW-VALUES
              MOVE  'RUEC7950'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'RUEC7950'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER-X        TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL  'BRAD7100'        USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
