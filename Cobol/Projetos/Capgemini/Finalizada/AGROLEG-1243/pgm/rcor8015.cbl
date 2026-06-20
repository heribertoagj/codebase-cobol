      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. RCOR8015.
       AUTHOR.     PRIME INFORMATICA.
      *================================================================*
      *                 P R I M E   I N F O R M A T I C A              *
      *----------------------------------------------------------------*
      *    PROGRAMA....: RCOR8015                                      *
      *    PROGRAMADOR.: PRIME INFORMATICA        - PRIME              *
      *    ANALISTA....: PRIME INFORMATICA        - PRIME              *
      *    DATA........: AGO/2012                                      *
      *----------------------------------------------------------------*
      *    OBJETIVO....: ESTE PROGRAMA COMPOE A CAMADA DE NEGOCIO PARA *
      *                  ATENDER A RECEPCAO DA MSG COR0001E ENVIADA    *
      *                  PELO BANCO CENTRAL.                           *
      *----------------------------------------------------------------*
      *    BOOKS.......:                                               *
      *    I#RCOR09 - AREA DE COMUNICACAO MODULOS IMS/CICS             *
      *    I#RCOR16 - AREA DE COMUNICACAO RCOR8015                     *
      *    I#RCOR13 - AREA DE COMUNICACAO RCOR6010                     *
      *    I#POOLB6 - AREA DE TRATAMENTO DE ERRO - CDES                *
      *    I#SIMM03 - BLOCO DE RETORNO                                 *
      *    I#SIMM04 - BCMSG                                            *
      *----------------------------------------------------------------*
      *    TABELAS.....:                                               *
      *    RCORB001 - DB2PRD.TPARM_SIST_CREDT                          *
      *================================================================*
      *                      A L T E R A C O E S                       *
      *================================================================*
PR0001*    PROGRAMADOR.:  HELIO SHOITI MORI                 -  PRIME   *
.     *    ANALISTA....:  RICARDO DALRI BOING               -  PRIME   *
.     *    DATA........:  19/03/2013                                   *
.     *    OBJETIVO....:  AJUSTE PARA INCLUSAO DO CAMPO                *
.     *                   NMSGEM_PROCS_CREDT NA TABELA RCORB002, QUE   *
.     *                   DEFINE O NUMERO DE SEQUENCIA DA MENSAGEM     *
PR0001*                   PARTICIONADA.                                *
      *----------------------------------------------------------------*
PR0013*    PROGRAMADOR.:  ELIEZER SIQUEIRA                  -  PRIME   *
-     *    ANALISTA....:  ELIEZER SIQUEIRA                  -  PRIME   *
-     *    DATA........:  OUTUBRO/2016                                 *
-     *    OBJETIVO....:  SE ERRO AO NOTIFICAR O LEGADO GRAVAR MOTIVO  *
PR0013*                   NA BASE RCORB002                             *
      *----------------------------------------------------------------*
AG1243*    PROGRAMADOR.:  HERIBERTO ANTONIO GIANNASI JUNIOR - 7COMM    *
AG1243*    ANALISTA....:  HERIBERTO ANTONIO GIANNASI JUNIOR - 7COMM    *
AG1243*    DATA........:  MAIO/2026                                     *
AG1243*----------------------------------------------------------------*
AG1243*    OBJETIVO....:  AGROLEG-1243 - NORMALIZAR ESPACOS DO CAMPO   *
AG1243*                   WKTPART (TAG {2210}) NA COR0001E ANTES DA    *
AG1243*                   PERSISTENCIA NO DB2 (RCORB002/RCOR6010).     *
AG1243*                   MAX 10000 BYTES POR OCORRENCIA {2210}.      *
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
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*  INICIO DA WORKING RCOR8015  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA AUXILIARES             *'.
      *----------------------------------------------------------------*

RDB    01  WRK-ERRO-CONSIST            PIC X(01)           VALUE SPACES.
       01  WRK-TAM                     PIC S9(04) COMP     VALUE ZEROS.
       01  WRK-POSICAO                 PIC 9(05)  COMP-3   VALUE ZEROS.
       01  IND-1                       PIC 9(05)  COMP-3   VALUE ZEROS.
       01  WRK-DATA                    PIC X(10)           VALUE SPACES.
       01  WRK-MODULO                  PIC X(08)           VALUE SPACES.
       01  WRK-MSG-COR0001E            PIC X(30000)        VALUE SPACES.

      *----------------------------------------------------------------*
AG1243*    AGROLEG-1243 - WKTPART {2210} : COMPACTACAO DE ESPACOS     *
      *----------------------------------------------------------------*
       01  WRK-EFF-LAST-MSG            PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-SCAN-DES            PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-I                   PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-J                   PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-POS-TAG2210         PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-CSTART              PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-HIT                 PIC  X(01)          VALUE SPACES.
       01  WRK-WKT-CONTINUA            PIC  X(01)          VALUE SPACES.
       01  WRK-NEXT-BRA                PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-IEND                PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-OLD-LEN             PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-NEW-LEN             PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-DELTA               PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-TAIL-L              PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-WKT-OUT-IX              PIC  9(05) COMP-3   VALUE ZEROS.
       01  WRK-CH-WKT                  PIC  X(01)          VALUE SPACES.
       01  WRK-WKT-PREV-SP             PIC  X(01)          VALUE SPACES.
       01  WRK-SP-REM                  PIC  9(05) COMP-3   VALUE ZEROS.
CG2605*    01  WRK-WKT-BUF                 PIC  X(12000)       VALUE SPACES.
CG2605*    01  WRK-WKT-BUF                 PIC  X(30000)       VALUE SPACES.
           01  WRK-WKT-BUF                 PIC  X(10000)       VALUE SPACES.
       01  WRK-OVERLAP-MOV             PIC  X(30000)       VALUE SPACES.

       01  WRK-NRO-CTRL-IF.
           03  WRK-BANCO               PIC 9(04)           VALUE ZEROS.
           03  WRK-CCUSTO              PIC X(04)           VALUE ZEROS.
           03  WRK-DT-JULIANA          PIC 9(05)           VALUE ZEROS.
           03  WRK-NUM-DOCTO           PIC 9(07)           VALUE ZEROS.

PR0001 01  WRK-NUM-09                  PIC 9(09)           VALUE ZEROS.
PR0001 01  FILLER                      REDEFINES WRK-NUM-09.
PR0001     03  WRK-NUM-06              PIC 9(06).
PR0001     03  WRK-NUM-03              PIC 9(03).

PR0013 01  WRK-CONTROLE-MSGEM-01       PIC X(80)           VALUE
PR0013     'LEGADO NAO PARAMETRIZADO NO SISTEMA RCOR'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA ERRO CICS/SISTEMA       *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERROS.
           03  WRK-MSG001              PIC  X(47)          VALUE
               'ERRO NO ACESSO AO MODULO RCOR6010'.
           03  WRK-MSG002.
               05  WRK-MSG002-TXT      PIC  X(26)          VALUE
                   'ERRO NO ACESSO AO MODULO: '.
               05  WRK-MSG002-PGM      PIC  X(08)          VALUE SPACES.
               05  FILLER              PIC  X(01)          VALUE SPACES.
               05  WRK-MSG002-AMB      PIC  X(12)          VALUE SPACES.

       01  WRK-MSGEM-ERRO-CICS.
           03  FILLER                  PIC  X(08)          VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO          PIC  9(03)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  FILLER                  PIC  X(10)          VALUE
               'EIBRESP = '.
           03  WRK-EIBRESP             PIC  9(04)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  WRK-TEXTO-ERRO          PIC  X(52)          VALUE SPACES.

       01  WRK-MSGEM-ERRO-MODULO.
           03  FILLER                  PIC  X(08)          VALUE
               'LOCAL = '.
           03  WRK-LOCAL-ERRO-MOD      PIC  9(03)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  FILLER                  PIC  X(14)          VALUE
               'COD RETORNO = '.
           03  WRK-COD-RETORNO         PIC  9(05)          VALUE ZEROS.
           03  FILLER                  PIC  X(01)          VALUE SPACES.
           03  WRK-TXT-ERRO-MOD        PIC  X(47)          VALUE SPACES.

PR0013 01  WRK-MSGEM-CICS.
-          03  FILLER                  PIC  X(30)          VALUE
-              'ERRO CICS NO ACESSO AO MODULO '.
-          03  WRK-MODULO-CICS         PIC  X(08)          VALUE SPACES.
-          03  FILLER                  PIC  X(13)          VALUE
-              ' - EIBRESP = '.
PR0013     03  WRK-EIBRESP-CICS        PIC  9(05)          VALUE ZEROS.

PR0013 01  WRK-MSGEM-MODULO.
-          03  FILLER                  PIC  X(15)          VALUE
-              'ERRO NO MODULO '.
-          03  WRK-MODULO-TAB          PIC  X(08)          VALUE SPACES.
-          03  FILLER                  PIC  X(18)          VALUE
-              ' - COD. RETORNO = '.
-          03  WRK-COD-RETORNO-TAB     PIC  9(05)          VALUE ZEROS.
-          03  FILLER                  PIC  X(10)          VALUE
-              ' - LOC. = '.
PR0013     03  WRK-LOCAL-TAB           PIC  X(03)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA PARA ERRO CICS / DB2    *'.
      *----------------------------------------------------------------*

       COPY 'I#POOLB6'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA COMUNICACAO IC30/IC31   *'.
      *----------------------------------------------------------------*

       01  WRK-SBIIC31.
           COPY 'I#RCOR17'.
           COPY 'I#RCOR18'.
           05  INP-MENSAGEM.
               07  INP-TRAN.
                   09  INP-TRANCODE        PIC  X(09)      VALUE SPACES.
                   09  FILLER              PIC  X(03)      VALUE SPACES.
               07  INP-DADOS.
                   09  INP-TIPO-CURSOR     PIC  9(01)      VALUE ZEROS.
                   09  INP-NRO-CTRLIF      PIC  X(20)      VALUE SPACES.
                   09  INP-NRO-UNIC-OPER   PIC  X(23)      VALUE SPACES.
                   09  INP-COD-MSGEM       PIC  X(09)      VALUE SPACES.
                   09  INP-COD-PROG        PIC  X(08)      VALUE SPACES.
PR0001             09  INP-SEQ-MSGEM       PIC  9(03)      VALUE ZEROS.
PR0001             09  INP-HINCL-REG       PIC  X(26)      VALUE SPACES.
               07  INP-RETORNO.
                   09  INP-COD-RETORNO     PIC  9(05)      VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA COMUNICACAO RCOR6010    *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR13'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA COMUNICACAO IMS/CICS    *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR09'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* AREA COMUNICACAO             *'.
      *----------------------------------------------------------------*

       COPY 'I#RCOR16'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* BLOCO DE RETORNO             *'.
      *----------------------------------------------------------------*

       COPY 'I#SIMM03'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '* BCMSG                        *'.
      *----------------------------------------------------------------*

       COPY 'I#SIMM04'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*        AREA DB2              *'.
      *----------------------------------------------------------------*

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE RCORB001
           END-EXEC.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING RCOR8015    *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  DFHCOMMAREA                 PIC X(30418).

      *================================================================*
       PROCEDURE                       DIVISION USING DFHCOMMAREA.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS HANDLE ABEND
               LABEL (9000-FORMATAR-ERRO-CICS)
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

           PERFORM 1000-FORMATAR-BLOCOS-MENSAGEM.

           PERFORM 2000-TRATAR-MSG-COR0001E.

           PERFORM 3000-RETORNAR-CHAMADOR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-FORMATAR-BLOCOS-MENSAGEM   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RCOR16-REGISTRO.

           MOVE ZEROS                  TO RCOR16-S-CD-RETORNO
                                          WRK-POSICAO
                                          IND-1.

           MOVE DFHCOMMAREA(1:EIBCALEN)
                                       TO RCOR16-REGISTRO.

           MOVE RCOR16-A-AREA-MSG(001:LENGTH OF SIMM03-REGISTRO)
                                       TO SIMM03-REGISTRO.

           COMPUTE WRK-POSICAO         = WRK-POSICAO + 1
                                       + LENGTH OF SIMM03-REGISTRO.

           MOVE RCOR16-A-AREA-MSG
                                (WRK-POSICAO:LENGTH OF SIMM04-REGISTRO)
                                       TO SIMM04-REGISTRO.

           COMPUTE WRK-POSICAO         = WRK-POSICAO
                                       + LENGTH OF SIMM04-REGISTRO.

           MOVE RCOR16-A-AREA-MSG(WRK-POSICAO:29731)
                                       TO WRK-MSG-COR0001E.

           PERFORM VARYING IND-1       FROM 30000 BY -1 UNTIL
                           IND-1       EQUAL ZEROS         OR
               WRK-MSG-COR0001E(IND-1:1)
                                       NOT EQUAL SPACES AND LOW-VALUES
           END-PERFORM.

AG1243**** AGROLEG-1243 - COMPACTAR ESPACOS WKTPART {2210}
           PERFORM 1050-NORMALIZA-WKT-WKTPART.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1050-NORMALIZA-WKT-WKTPART      SECTION.
      *----------------------------------------------------------------*
AG1243**** AGROLEG-1243 - CONTROLA A COMPACTACAO DO WKTPART {2210}
AG1243**** VALIDA PRE-CONDICOES E ACIONA O LACO DE BUSCA/COMPACTACAO

           MOVE IND-1                  TO WRK-EFF-LAST-MSG.

           IF  WRK-EFF-LAST-MSG        EQUAL ZEROS
            OR WRK-EFF-LAST-MSG        LESS THAN +7
            OR WRK-EFF-LAST-MSG        GREATER THAN +29994
               GO                      TO 1050-99-FIM.

           MOVE +1                     TO WRK-WKT-SCAN-DES.
           MOVE 'S'                    TO WRK-WKT-CONTINUA.

           PERFORM 1051-WKT-BUSCA-TAG
               UNTIL WRK-WKT-CONTINUA  NOT EQUAL 'S'.

      *----------------------------------------------------------------*
       1050-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1051-WKT-BUSCA-TAG              SECTION.
      *----------------------------------------------------------------*
AG1243**** AGROLEG-1243 - VARRE MSG BUSCANDO A TAG {2210}
AG1243**** SE ENCONTRAR ACIONA 1052 PARA LOCALIZAR FIM E COMPACTAR
AG1243**** SE NAO ENCONTRAR ENCERRA O LACO PRINCIPAL (CONTINUA='N')

           MOVE 'N'                    TO WRK-WKT-HIT.

           PERFORM VARYING WRK-WKT-I
                         FROM WRK-WKT-SCAN-DES BY +1
                         UNTIL WRK-WKT-I   GREATER THAN WRK-EFF-LAST-MSG
                            OR WRK-WKT-HIT  EQUAL 'S'

               IF  WRK-MSG-COR0001E(WRK-WKT-I:6)
                                        EQUAL '{2210}'
                   MOVE 'S'            TO WRK-WKT-HIT
                   MOVE WRK-WKT-I      TO WRK-WKT-POS-TAG2210
                   COMPUTE WRK-WKT-CSTART
                                        = WRK-WKT-I + 6
               END-IF
           END-PERFORM.

           IF  WRK-WKT-HIT             EQUAL 'N'
               MOVE 'N'                TO WRK-WKT-CONTINUA
               GO                      TO 1051-99-FIM
           END-IF.

           PERFORM 1052-WKT-LOCALIZA-FIM.

      *----------------------------------------------------------------*
       1051-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1052-WKT-LOCALIZA-FIM           SECTION.
      *----------------------------------------------------------------*
AG1243**** AGROLEG-1243 - LOCALIZA O FIM DO SEGMENTO WKTPART
AG1243**** ACIONA A COMPACTACAO E DESLOCA O RESTANTE DA MENSAGEM

           MOVE 'N'                    TO WRK-WKT-HIT.
           MOVE ZEROS                  TO WRK-NEXT-BRA.

           PERFORM VARYING WRK-WKT-J
                         FROM WRK-WKT-CSTART BY +1
                         UNTIL WRK-WKT-J   GREATER THAN WRK-EFF-LAST-MSG
                            OR WRK-WKT-HIT  EQUAL 'S'

               IF  WRK-MSG-COR0001E(WRK-WKT-J:1)
                                        EQUAL '{'
                   MOVE 'S'            TO WRK-WKT-HIT
                   MOVE WRK-WKT-J      TO WRK-NEXT-BRA
               END-IF

           END-PERFORM.

           IF  WRK-NEXT-BRA            EQUAL ZEROS
               COMPUTE WRK-NEXT-BRA    = WRK-EFF-LAST-MSG + 1
           END-IF.

           COMPUTE WRK-WKT-IEND        = WRK-NEXT-BRA - 1.
           COMPUTE WRK-WKT-OLD-LEN     = WRK-WKT-IEND
                                       - WRK-WKT-CSTART + 1.

           PERFORM 1053-WKT-COMPACTAR-SEGDO.

           IF  WRK-WKT-DELTA           GREATER THAN ZEROS

               COMPUTE WRK-WKT-TAIL-L  = WRK-EFF-LAST-MSG
                                       - WRK-WKT-IEND

               IF  WRK-WKT-TAIL-L      GREATER THAN ZEROS
                   MOVE WRK-MSG-COR0001E
                        (WRK-NEXT-BRA:WRK-WKT-TAIL-L)
                                        TO WRK-OVERLAP-MOV
                   COMPUTE WRK-WKT-OUT-IX
                                        = WRK-WKT-CSTART
                                        + WRK-WKT-NEW-LEN
                   MOVE WRK-OVERLAP-MOV(1:WRK-WKT-TAIL-L)
                                         TO WRK-MSG-COR0001E
                                         (WRK-WKT-OUT-IX:WRK-WKT-TAIL-L)
               END-IF

               COMPUTE WRK-EFF-LAST-MSG = WRK-EFF-LAST-MSG
                                        - WRK-WKT-DELTA
               MOVE WRK-EFF-LAST-MSG    TO IND-1
               COMPUTE WRK-WKT-OUT-IX   = WRK-EFF-LAST-MSG + 1
               MOVE SPACES              TO WRK-MSG-COR0001E
                                          (WRK-WKT-OUT-IX:WRK-WKT-DELTA)

           END-IF.

           COMPUTE WRK-WKT-SCAN-DES     = WRK-WKT-CSTART
                                        + WRK-WKT-NEW-LEN.

      *----------------------------------------------------------------*
       1052-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       1053-WKT-COMPACTAR-SEGDO        SECTION.
      *----------------------------------------------------------------*
AG1243**** AGROLEG-1243 - COLAPS ESPACOS DUPLOS NO SEGMENTO WKTPART
AG1243**** ENTRADA: WRK-WKT-CSTART / WRK-WKT-IEND / WRK-WKT-OLD-LEN
AG1243**** SAIDA  : WRK-WKT-NEW-LEN / WRK-WKT-DELTA
AG1243**** LIMITE : 10000 BYTES POR OCORRENCIA DO ATRIBUTO {2210}

           IF  WRK-WKT-OLD-LEN         EQUAL ZEROS
CG2605*     OR WRK-WKT-OLD-LEN         GREATER THAN 12000
CG2605*     OR WRK-WKT-OLD-LEN         GREATER THAN 30000
            OR WRK-WKT-OLD-LEN         GREATER THAN 10000
               MOVE WRK-WKT-OLD-LEN    TO WRK-WKT-NEW-LEN
               MOVE ZEROS              TO WRK-WKT-DELTA
               GO                      TO 1053-99-FIM.

           MOVE ZEROS                  TO WRK-WKT-NEW-LEN
                                          WRK-SP-REM.
           MOVE SPACES                 TO WRK-WKT-PREV-SP
                                          WRK-WKT-BUF.

           PERFORM VARYING WRK-WKT-OUT-IX
                         FROM WRK-WKT-CSTART BY +1
                        UNTIL WRK-WKT-OUT-IX GREATER THAN WRK-WKT-IEND

               MOVE WRK-MSG-COR0001E(WRK-WKT-OUT-IX:1)
                                       TO WRK-CH-WKT

               IF  WRK-CH-WKT          EQUAL SPACES
               AND WRK-WKT-PREV-SP     EQUAL SPACES
                   ADD +1              TO WRK-SP-REM
               ELSE
                   ADD +1               TO WRK-WKT-NEW-LEN
                   MOVE WRK-CH-WKT
                        TO WRK-WKT-BUF(WRK-WKT-NEW-LEN:1)
               END-IF

               MOVE WRK-CH-WKT         TO WRK-WKT-PREV-SP

           END-PERFORM.

           IF  WRK-WKT-NEW-LEN         GREATER THAN ZEROS
               MOVE WRK-WKT-BUF(1:WRK-WKT-NEW-LEN)
                                         TO WRK-MSG-COR0001E
                                        (WRK-WKT-CSTART:WRK-WKT-NEW-LEN)
           END-IF.

           COMPUTE WRK-WKT-DELTA        = WRK-WKT-OLD-LEN
                                        - WRK-WKT-NEW-LEN.

      *----------------------------------------------------------------*
       1053-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-TRATAR-MSG-COR0001E        SECTION.
      *----------------------------------------------------------------*

RDB        MOVE 'N'                    TO WRK-ERRO-CONSIST.

           PERFORM 2100-ACESSAR-RCOR6010.

           PERFORM 2200-SELECT-RCORB001.

           IF (SQLCODE                 EQUAL ZEROS)
               IF (CINDCD-AMBTE-SIST   EQUAL 1)
                   PERFORM 2300-CHAMAR-PGM-LEGADO-IMS
               ELSE
                   PERFORM 2400-CHAMAR-PGM-LEGADO-CICS
               END-IF
           ELSE
PR0013         MOVE SPACES             TO RCOR13-DADOS-COMPL
-              MOVE 1                  TO RCOR13-CTPO-ERRO-MSGEM
-              MOVE WRK-CONTROLE-MSGEM-01
-                                      TO RCOR13-RERRO-MSGEM-CREDT
PR0013         PERFORM 4000-ATUALIZA-IND-ENVIO-LEGADO
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-ACESSAR-RCOR6010           SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO RCOR13-REGISTRO.

           INITIALIZE RCOR13-REGISTRO.

           MOVE WRK-MSG-COR0001E(18:20)
                                       TO RCOR13-CNRO-CTRL-INSTC
                                          WRK-NRO-CTRL-IF.

           IF (WRK-CCUSTO              EQUAL 'TMNF')
               MOVE 6                  TO RCOR13-TP-INCL
           ELSE
               MOVE 2                  TO RCOR13-TP-INCL
           END-IF.

PRI371     MOVE WRK-MSG-COR0001E(1:9)  TO RCOR13-COD-MSGEM.
           MOVE WRK-MSG-COR0001E       TO RCOR13-MSGEM-CREDT-TXT.
HSM        IF  SIMM04-CNRO-SEQ-BACEN   EQUAL ZEROS
.              MOVE 1                  TO SIMM04-CNRO-SEQ-BACEN
HSM        END-IF.
PR0001     MOVE SIMM04-CNRO-SEQ-BACEN  TO WRK-NUM-09.
PR0001     MOVE WRK-NUM-03             TO RCOR13-SEQ-MSG.

           EXEC CICS LINK
                PROGRAM ('RCOR6010')
                COMMAREA(RCOR13-REGISTRO)
                LENGTH  (LENGTH OF RCOR13-REGISTRO)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE WRK-MSG001         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 10                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
               PERFORM 3000-RETORNAR-CHAMADOR
           END-IF.

           IF (RCOR13-S-COD-RETORNO    NOT EQUAL ZEROS)
RDB            IF (RCOR13-S-COD-RETORNO
.                                      EQUAL 10)
.                  MOVE 'S'            TO WRK-ERRO-CONSIST
RDB            END-IF
               MOVE WRK-MSG001         TO WRK-TXT-ERRO-MOD
               MOVE 'RCOR6010'         TO WRK-MODULO
               MOVE RCOR13-S-COD-RETORNO
                                       TO WRK-COD-RETORNO
               MOVE 20                 TO WRK-LOCAL-ERRO-MOD
               PERFORM 9300-FORMATAR-ERRO-MODULO
               PERFORM 3000-RETORNAR-CHAMADOR
           END-IF.

           EXEC CICS
               SYNCPOINT
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-SELECT-RCORB001            SECTION.
      *----------------------------------------------------------------*

           EXEC SQL
               SET :WRK-DATA = CURRENT DATE
           END-EXEC.

           MOVE WRK-MSG-COR0001E(18:20)
                                       TO WRK-NRO-CTRL-IF.
           MOVE WRK-CCUSTO             TO CSIST            OF RCORB001.
           MOVE WRK-DATA               TO DINIC-VGCIA-PARM OF RCORB001.

           EXEC SQL
               SELECT CINDCD_AMBTE_SIST,
                      CPROG
                 INTO :RCORB001.CINDCD-AMBTE-SIST,
                      :RCORB001.CPROG
                 FROM DB2PRD.TPARM_SIST_CREDT
                WHERE CSIST             = :RCORB001.CSIST
                  AND DINIC_VGCIA_PARM <= :RCORB001.DINIC-VGCIA-PARM
             ORDER BY DINIC_VGCIA_PARM DESC
                FETCH FIRST 1 ROWS ONLY
           END-EXEC.

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE 'TPARM_SIST_CREDT' TO ERR-DBD-TAB
               MOVE 'SELECT'           TO ERR-FUN-COMANDO
               MOVE SQLCODE            TO ERR-SQL-CODE
               MOVE '30'               TO ERR-LOCAL
               MOVE SQLCA              TO ERR-SQLCA
               PERFORM 9200-FORMATAR-ERRO-DB2
               PERFORM 3000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-CHAMAR-PGM-LEGADO-IMS      SECTION.
      *----------------------------------------------------------------*

           INITIALIZE INP-MENSAGEM.

           COMPUTE WRK-TAM             = LENGTH OF HPD2000C     +
                                         LENGTH OF IC30-COM     +
                                         LENGTH OF INP-MENSAGEM +
PR0001                                   1803.

           MOVE 'IC31'                 TO HPD-TRX.
           MOVE 'H03'                  TO HPD-ID.
           MOVE SPACES                 TO HPD-NUM-PERIF.
           MOVE ZEROS                  TO HPD-PERIF-ANT.
           MOVE ZEROS                  TO HPD-LU-RESPOSTA.
           MOVE ZEROS                  TO HPD-AG-ORIGEM.
           MOVE EIBTASKN               TO HPD-SEQ-PERIF.
           MOVE ZEROS                  TO HPD-AG-DESTINO.
           MOVE SPACES                 TO HPD-ANULACAO.
           MOVE ZEROS                  TO HPD-COD-PERIF.
           MOVE ZEROS                  TO HPD-CTRLE-TX.
           MOVE ZEROS                  TO HPD-RCODE1.
           MOVE 'N'                    TO IC30-FUNCAO.
           MOVE SPACES                 TO IC30-RCODE.
           MOVE ZEROS                  TO IC30-ID-PERIF.
           MOVE WRK-TAM                TO IC30-TEXT-LEN.
           MOVE 'RCOR5025'             TO INP-TRANCODE.
           MOVE 1                      TO INP-TIPO-CURSOR.
           MOVE WRK-MSG-COR0001E(18:20)
                                       TO INP-NRO-CTRLIF.
           MOVE SPACES                 TO INP-NRO-UNIC-OPER.
PRI371     MOVE WRK-MSG-COR0001E(1:9)  TO INP-COD-MSGEM.
           MOVE CPROG OF RCORB001      TO INP-COD-PROG.
HSM        IF  SIMM04-CNRO-SEQ-BACEN   EQUAL ZEROS
.              MOVE 1                  TO SIMM04-CNRO-SEQ-BACEN
HSM        END-IF.
PR0001     MOVE SIMM04-CNRO-SEQ-BACEN  TO WRK-NUM-09.
PR0001     MOVE WRK-NUM-03             TO INP-SEQ-MSGEM.
PR0001     MOVE RCOR13-S-HINCL-REG     TO INP-HINCL-REG.

           EXEC CICS LINK
                PROGRAM ('SBIIC31')
                COMMAREA(WRK-SBIIC31)
                LENGTH  (LENGTH OF WRK-SBIIC31)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE 'SBIIC31'          TO WRK-MSG002-PGM
                                          WRK-MODULO
PR0013                                    WRK-MODULO-CICS
               MOVE 'AMBIENTE.IMS'     TO WRK-MSG002-AMB
               MOVE WRK-MSG002         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
PR0013                                    WRK-EIBRESP-CICS
               MOVE 40                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
PR0013         MOVE SPACES             TO RCOR13-DADOS-COMPL
-              MOVE 1                  TO RCOR13-CTPO-ERRO-MSGEM
PR0013         MOVE WRK-MSGEM-CICS     TO RCOR13-RERRO-MSGEM-CREDT
               PERFORM 4000-ATUALIZA-IND-ENVIO-LEGADO
               MOVE ZEROS              TO RCOR16-S-CD-RETORNO
               GO                      TO 2300-99-FIM
           END-IF.

           IF (IC30-RCODE              EQUAL 'O')
               NEXT SENTENCE
           ELSE
               MOVE 'SBIIC31'          TO WRK-MSG002-PGM
                                          WRK-MODULO
PR0013                                    WRK-MODULO-TAB
               MOVE 'AMBIENTE.IMS'     TO WRK-MSG002-AMB
               MOVE WRK-MSG002         TO WRK-TXT-ERRO-MOD
               MOVE 888                TO WRK-COD-RETORNO
PR0013                                    WRK-COD-RETORNO-TAB
               MOVE 50                 TO WRK-LOCAL-ERRO-MOD
PR0013         MOVE '050'              TO WRK-LOCAL-TAB
               PERFORM 9300-FORMATAR-ERRO-MODULO
PR0013         MOVE SPACES             TO RCOR13-DADOS-COMPL
-              MOVE 1                  TO RCOR13-CTPO-ERRO-MSGEM
PR0013         MOVE WRK-MSGEM-MODULO   TO RCOR13-RERRO-MSGEM-CREDT
               PERFORM 4000-ATUALIZA-IND-ENVIO-LEGADO
               MOVE ZEROS              TO RCOR16-S-CD-RETORNO
               GO                      TO 2300-99-FIM
           END-IF.

           IF (INP-COD-RETORNO         NOT EQUAL ZEROS)
               MOVE 'SBIIC31'          TO WRK-MSG002-PGM
                                          WRK-MODULO
PR0013                                    WRK-MODULO-TAB
               MOVE 'AMBIENTE.IMS'     TO WRK-MSG002-AMB
               MOVE WRK-MSG002         TO WRK-TXT-ERRO-MOD
               MOVE INP-COD-RETORNO    TO WRK-COD-RETORNO
               MOVE 55                 TO WRK-LOCAL-ERRO-MOD
               PERFORM 9300-FORMATAR-ERRO-MODULO
PR0013         MOVE SPACES             TO RCOR13-DADOS-COMPL
-              MOVE 1                  TO RCOR13-CTPO-ERRO-MSGEM
-              MOVE WRK-MSGEM-ERRO-MODULO
PR0013                                 TO RCOR13-RERRO-MSGEM-CREDT
               PERFORM 4000-ATUALIZA-IND-ENVIO-LEGADO
               MOVE ZEROS              TO RCOR16-S-CD-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-CHAMAR-PGM-LEGADO-CICS     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RCOR09-REGISTRO.

PRI371     MOVE WRK-MSG-COR0001E(1:9)  TO RCOR09-COD-MSG.
           MOVE WRK-MSG-COR0001E(18:20)
                                       TO RCOR09-NRO-CTRL-IF.
           MOVE IND-1                  TO RCOR09-TAM-SISMSG.
           MOVE WRK-MSG-COR0001E       TO RCOR09-SISMSG
HSM        IF  SIMM04-CNRO-SEQ-BACEN   EQUAL ZEROS
.              MOVE 1                  TO SIMM04-CNRO-SEQ-BACEN
HSM        END-IF.
PR0001     MOVE SIMM04-CNRO-SEQ-BACEN  TO WRK-NUM-09.
PR0001     MOVE WRK-NUM-03             TO RCOR09-SEQ-MSG.

           MOVE CPROG OF RCORB001      TO WRK-MODULO.

           EXEC CICS LINK
                PROGRAM (WRK-MODULO)
                COMMAREA(RCOR09-REGISTRO)
                LENGTH  (LENGTH OF RCOR09-REGISTRO)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE WRK-MODULO         TO WRK-MSG002-PGM
PR0013                                    WRK-MODULO-CICS
               MOVE 'AMBIENT.CICS'     TO WRK-MSG002-AMB
               MOVE WRK-MSG002         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
PR0013                                    WRK-EIBRESP-CICS
               MOVE 60                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
PR0013         MOVE SPACES             TO RCOR13-DADOS-COMPL
-              MOVE 1                  TO RCOR13-CTPO-ERRO-MSGEM
PR0013         MOVE WRK-MSGEM-CICS     TO RCOR13-RERRO-MSGEM-CREDT
               PERFORM 4000-ATUALIZA-IND-ENVIO-LEGADO
               MOVE ZEROS              TO RCOR16-S-CD-RETORNO
               GO                      TO 2400-99-FIM
           END-IF.

           IF (RCOR09-COD-RETORNO      NOT EQUAL ZEROS)
               MOVE WRK-MODULO         TO WRK-MSG002-PGM
PR0013                                    WRK-MODULO-TAB
               MOVE 'AMBIENT.CICS'     TO WRK-MSG002-AMB
               MOVE WRK-MSG002         TO WRK-TXT-ERRO-MOD
               MOVE RCOR09-COD-RETORNO TO WRK-COD-RETORNO
PR0013                                    WRK-COD-RETORNO-TAB
               MOVE 70                 TO WRK-LOCAL-ERRO-MOD
PR0013         MOVE '070'              TO WRK-LOCAL-TAB
               PERFORM 9300-FORMATAR-ERRO-MODULO
PR0013         MOVE SPACES             TO RCOR13-DADOS-COMPL
-              MOVE 1                  TO RCOR13-CTPO-ERRO-MSGEM
PR0013         MOVE WRK-MSGEM-MODULO   TO RCOR13-RERRO-MSGEM-CREDT
               PERFORM 4000-ATUALIZA-IND-ENVIO-LEGADO
               MOVE ZEROS              TO RCOR16-S-CD-RETORNO
           END-IF.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE RCOR16-REGISTRO        TO DFHCOMMAREA(1:EIBCALEN).

           EXEC CICS
               RETURN
           END-EXEC.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-ATUALIZA-IND-ENVIO-LEGADO  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE RCOR13-CONTROLE.

           MOVE 7                      TO RCOR13-TP-INCL.
           MOVE RCOR13-S-CMSGEM-CREDT  TO RCOR13-E-CMSGEM-CREDT.
           MOVE RCOR13-S-CCTRL-MSGEM   TO RCOR13-E-CCTRL-MSGEM.
           MOVE RCOR13-S-HINCL-REG     TO RCOR13-E-HINCL-REG.
HSM        IF  SIMM04-CNRO-SEQ-BACEN   EQUAL ZEROS
.              MOVE 1                  TO SIMM04-CNRO-SEQ-BACEN
HSM        END-IF.
PR0001     MOVE SIMM04-CNRO-SEQ-BACEN  TO WRK-NUM-09.
PR0001     MOVE WRK-NUM-03             TO RCOR13-SEQ-MSG.

           EXEC CICS LINK
                PROGRAM ('RCOR6010')
                COMMAREA(RCOR13-REGISTRO)
                LENGTH  (LENGTH OF RCOR13-REGISTRO)
                NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
               MOVE WRK-MSG001         TO WRK-TEXTO-ERRO
               MOVE EIBRESP            TO WRK-EIBRESP
               MOVE 80                 TO WRK-LOCAL-ERRO
               PERFORM 9000-FORMATAR-ERRO-CICS
               PERFORM 3000-RETORNAR-CHAMADOR
           END-IF.

           IF (RCOR13-S-COD-RETORNO    NOT EQUAL ZEROS)
RDB            IF (RCOR13-S-COD-RETORNO
.                                      EQUAL 10)
.                  MOVE 'S'            TO WRK-ERRO-CONSIST
RDB            END-IF
               MOVE WRK-MSG001         TO WRK-TXT-ERRO-MOD
               MOVE 'RCOR6010'         TO WRK-MODULO
               MOVE RCOR13-S-COD-RETORNO
                                       TO WRK-COD-RETORNO
               MOVE 90                 TO WRK-LOCAL-ERRO-MOD
               PERFORM 9300-FORMATAR-ERRO-MODULO
               PERFORM 3000-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9000-FORMATAR-ERRO-CICS         SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE SPACES                 TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR8015'             TO ERR-PGM.
           MOVE WRK-MSGEM-ERRO-CICS    TO ERR-TEXTO.
           MOVE 'RCOR8015'             TO ERR-MODULO.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.
           MOVE 99                     TO RCOR16-S-CD-RETORNO.

           EXEC CICS ASSIGN
                     APPLID(ERR-CICS)
           END-EXEC.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9100-GRAVAR-LOG-CDES            SECTION.
      *----------------------------------------------------------------*

           EXEC CICS START
               TRANSID('CD11')
               FROM   (ERRO-AREA)
               LENGTH (+520)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 EQUAL DFHRESP(NORMAL))
               CONTINUE
           END-IF.

      *----------------------------------------------------------------*
       9100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9200-FORMATAR-ERRO-DB2          SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.
           MOVE 'ERRO DB2'             TO ERR-TEXTO-MSG.
           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'DB2'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR8015'             TO ERR-PGM
                                          ERR-MODULO.
           MOVE SPACES                 TO ERR-SEGM
                                          ERR-COD-USER
                                          ERR-COD-DEPTO
                                          ERR-DFHEIBLK
                                          ERR-TERMINAL
                                          ERR-CICS.
           MOVE 99                     TO RCOR16-S-CD-RETORNO.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9300-FORMATAR-ERRO-MODULO       SECTION.
      *----------------------------------------------------------------*

           MOVE 'VRS001'               TO ERR-VERSAO.
           MOVE ZEROS                  TO ERR-COD-MSG.

RDB        IF (WRK-ERRO-CONSIST        EQUAL 'S')
.              MOVE RCOR13-S-DSC-ERRO  TO ERR-TEXTO-MSG
.          ELSE
.              MOVE WRK-MSGEM-ERRO-MODULO
.                                      TO ERR-TEXTO-MSG
RDB        END-IF.

           MOVE ZEROS                  TO ERR-RETURN-CODE.
           MOVE 'APL'                  TO ERR-TIPO-ACESSO.
           MOVE 'RCOR8015'             TO ERR-PGM.
           MOVE SPACES                 TO ERR-TEXTO.
           MOVE WRK-MODULO             TO ERR-MODULO.
           MOVE DFHEIBLK               TO ERR-DFHEIBLK.
           MOVE 99                     TO RCOR16-S-CD-RETORNO.

           EXEC CICS ASSIGN
                     APPLID(ERR-CICS)
           END-EXEC.

           PERFORM 9100-GRAVAR-LOG-CDES.

      *----------------------------------------------------------------*
       9300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
