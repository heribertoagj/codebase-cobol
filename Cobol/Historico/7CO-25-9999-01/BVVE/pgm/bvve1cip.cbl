      *================================================================*
      *    PROGRAMA....: BVVE1CIP                                      *
      *    ANALISTA....: Luis Felipe                   - 7COMM         *
      *    DATA........: 26/07/2020                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: CONEXAO COM A CIP                             *
      *----------------------------------------------------------------*
      *    BCO DE DADOS:                                               *
      *    DB2 TABLE                                   INCLUDE/BOOK    *
      *----------------------------------------------------------------*
      * BOOKS FUNCIONAIS:                                              *
      *                                                                *
      * BVVEW999 - AREA DE INTERFACE COM PROGRAMA BATCH CICLICA        *
      * --                                                             *
      * BOOK'S ARQUITETURAIS.                                          *
      * I#FRWKAQ - COMMAREA ENTRE PROGRAMA/FRAMEWORK                   *
      * I#FRWK04 - COMMAREA ENTRE PROGRAMA/FRWK1200(GAM)               *
      * I#FRWKGE - COMMAREA FRWK1999 (LOG DE ERRO)                     *
      * I#FRWKMD - COMMAREA FRWK1999 (LOG DE ERROS MODULO)             *
      * I#FRWKCI - COMMAREA FRWK1999 (LOG DE ERROS CICS)               *
      * I#FRWKD2 - COMMAREA FRWK1999 (LOG DE ERROS DB2)                *
7C1121*================================================================*
.     *  ALTERACAO                                                     *
.     *================================================================*
.     * ANALISTA: EDSON MATSUMOTO                                      *
.     * DATA: 08/11/2021                                               *
.     * OBJETIVO: INCLUIR O CAMPO SERIE (kid_certificado) NO HEADER    *
.     * DAS MENSAGENS                                                  *
7C1121*================================================================*
TON01 *----------------------------------------------------------------*
      *                     A L T E R A C A O                          *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMADOR.: CLERISTON MARZENDA                -TON01      *
      *    ANALISTA....: JOSEMAR SIVEK                                 *
      *    DATA........: 26/06/2023                                    *
      *    OBJETIVO....: ENVIO DE NOVOS CAMPOS NA MENSAGEM BVVEWE10    *
      *                          CAMPOS: BVVEWE10-INTEROPERABILIDADE   *
TON01 *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BVVE1CIP.
       AUTHOR.     7COMM.

       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE                 SECTION.
       01 WRK-GLOG.
          05 WRK-MONITOR               PIC  X(10244)  VALUE SPACES.
          05 FILLER                    PIC  X(10755)  VALUE SPACES.
       77 WRK-PARM1                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM2                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM3                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM4                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM5                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM6                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM7                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM8                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM9                    PIC  X(160)        VALUE SPACES.
       77 WRK-PARM10                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM11                   PIC  X(160)        VALUE SPACES.
       01 WRK-PARM12.
         05 WRK-PARM12-9               PIC  9(012)V99     VALUE ZEROS.
       77 WRK-PARM13                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM14                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM15                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM16                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM17                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM18                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM19                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM20                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM21                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM22                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM23                   PIC  X(160)        VALUE SPACES.
       77 WRK-PARM24                   PIC  X(160)        VALUE SPACES.
TON01  77 WRK-PARM25                   PIC  X(160)        VALUE SPACES.
TON01  77 WRK-PARM26                   PIC  X(160)        VALUE SPACES.

       77 WRK-PARM99                   PIC  X(160)        VALUE SPACES.
       77 WRK-VAZIO                    PIC  X(018)        VALUE SPACES.
7C1121 77 WRK-SERIE                    PIC  X(032)        VALUE SPACES.
       77 WRK-TAM-JSON                 PIC S9(005) COMP-3  VALUE ZEROS.
       77 WRK-TAM-COBOL                PIC S9(005) COMP-3  VALUE ZEROS.
       77 WRK-SOMA-TAMANHO             PIC S9(005) COMP-3  VALUE ZEROS.
       77 WRK-FRWK1999                 PIC  X(008) VALUE 'FRWK1999'.
       77 WRK-PROGRAMA                 PIC  X(008) VALUE 'BVVE1CIP'.
       77 WRK-FRWK1200                 PIC  X(008) VALUE 'FRWK1200'.

       77 WRK-FRWK1500                 PIC  X(008) VALUE 'FRWK1500'.
       77 WRK-PRIM-QUERY               PIC  X(001)        VALUE SPACES.
       77 WRK-URL-AUX                  PIC  X(256)        VALUE SPACES.
       77 WRK-QS-AUX                   PIC  X(256)        VALUE SPACES.
       77 WRK-URL-CALLBACK             PIC  X(256)        VALUE SPACES.
       77 WRK-COD-COMUNIC              PIC  9(002)        VALUE ZEROS.
       77 WRK-APPLID                   PIC  X(008)        VALUE SPACES.
MB0501 01 WRK-ERRO-EEXT.
MB0501    05 WRK-ERRO-EEXT-NUM         PIC  9(004)        VALUE ZEROS.

       01  FILLER                      PIC X(050)         VALUE
           '*** INICIO DA WORKING BVVE1CIP ***'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE COMUNICACAO DO FRWK1999 (LOG DE ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-ERRO.
           COPY 'I#FRWKGE'.
           05 WRK-BLOCO-INFO-ERRO.
             10 WRK-CHAR-INFO-ERRO     PIC X(01) OCCURS 0
                                       TO 30000 TIMES
                                       DEPENDING ON FRWKGHEA-TAM-DADOS.
      *
LBL   *----------------------------------------------------------------*
.      01  FILLER                      PIC  X(050)         VALUE
.          'AREA DE ERRO DB2'.
.     *----------------------------------------------------------------*
.      01  WRK-ERRO-DB2.
.          COPY I#FRWKDB.
LBL   *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(032)         VALUE
           '*** AREA COMUNICACAO - BVVE3519 '.
      *----------------------------------------------------------------*
       01  WRK-BVVE3519-COMMAREA.
           COPY 'BVVEWM20'.
           COPY 'BVVEWM21'.
           COPY 'BVVEWM22'.

      *----------------------------------------------------------------*
      *    AREA DOS MAPAS                                              *
      *----------------------------------------------------------------*
           COPY BVVEWMAP.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO MODULO (API-ERRO)'.
      *----------------------------------------------------------------*
       01  WRK-COPY-MOD.
           COPY 'I#FRWKMD'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(50)   VALUE
           'AREA DE TRATAMENTO DE ERRO LIVRE(API-ERRO)'.
      *----------------------------------------------------------------*
       01 WRK-AREA-ERRO-LIVRE.
          COPY 'I#FRWKLI'.
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE INTERFACE (GAM)'.
      *----------------------------------------------------------------*
       01  WRK-AREA-FRWK1200.
           COPY 'I#FRWK04'.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC X(050)      VALUE
           'AREA DE TRATAMENTO DE ERRO GLOG'.
      *----------------------------------------------------------------*
       01  WRK-COPY-GLOG.
           COPY FRWKW99B.
      *----------------------------------------------------------------*

      ***  COMMAREA PARA ACIONAMENTO DA API FRWK1200
       01  WRK-FRWKW203.
           COPY  FRWKW203.

      ***  COMMAREA PARA ACIONAMENTO DA API FRWK1500
       01  WRK-FRWKW500.
           COPY FRWKW500.

      ***  COMMAREA PARA ACIONAMENTO DE SERVICO EXTERNO (EEXT)
       01  WRK-EEXTW301.
           COPY EEXTW301.

      *----------------------------------------------------------------*
      *    AREA DE COMUNICACAO COM EEXT PARA GERACAO DO HEADER         *
      *----------------------------------------------------------------*
       01  WRK-EEXTWBLH.
           COPY EEXTWBLH.

7C1121*----------------------------------------------------------------*
.      01  FILLER                      PIC X(032)          VALUE
.          '* AREA DE DB2 *'.
.     *----------------------------------------------------------------*
.
.          EXEC SQL
.              INCLUDE SQLCA
.          END-EXEC.
.
.          EXEC SQL
.              INCLUDE BVVEB057
.          END-EXEC.
7C1121
       01  FILLER                      PIC X(032)      VALUE
           '*** FIM DA WORKING BVVE1CIP ***'.

       01  WRK-BVVEWE00.
           COPY BVVEWE00.
           COPY BVVEWE01.
           COPY BVVEWE10.
           COPY BVVEWE11.
           COPY BVVEWE13.
           COPY BVVEWE19.
           COPY BVVEWE20.
           COPY BVVEWE21.
           COPY BVVEWE25.
           COPY BVVEWE27.
TM         COPY BVVEWX27.

       LINKAGE                         SECTION.
       01  DFHCOMMAREA.
         02  LNK-AREA-IFRWKAQ.
           COPY 'I#FRWKAQ'.

       PROCEDURE DIVISION              USING DFHCOMMAREA.
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA.                               *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR

           PERFORM 2000-PROCESSAR

           PERFORM 4000-FINALIZAR.

       0000-99-FIM. EXIT.

      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.

           INITIALIZE              FRWKGHEA-REGISTRO
                                   FRWKGMOD-REGISTRO
                                   REPLACING NUMERIC BY ZEROS
                                        ALPHANUMERIC BY SPACES.

           PERFORM 1050-RECUPERA-GAM.
           PERFORM 1100-FORMATA-URL.

7C1121     PERFORM 1200-ACESSAR-BVVEB057.

           EVALUATE BVVEWE00-LEIAUTE
               WHEN 'BVVEWE01'
                  MOVE 01     TO WRK-COD-COMUNIC
               WHEN 'BVVEWE10'
                  MOVE 02     TO WRK-COD-COMUNIC
               WHEN 'BVVEWE11'
                  MOVE 03     TO WRK-COD-COMUNIC
               WHEN 'BVVEWE13'
                  MOVE 04     TO WRK-COD-COMUNIC
               WHEN 'BVVEWE19'
                  EVALUATE BVVEWE19-TP-COMU
                      WHEN 1
                         MOVE 07     TO WRK-COD-COMUNIC
                      WHEN 2
                         MOVE 08     TO WRK-COD-COMUNIC
                      WHEN 3
                         MOVE 09     TO WRK-COD-COMUNIC
                      WHEN 4
                         MOVE 10     TO WRK-COD-COMUNIC
                      WHEN 5
                         MOVE 12     TO WRK-COD-COMUNIC
                      WHEN 6
                         MOVE 13     TO WRK-COD-COMUNIC
                      WHEN OTHER
                         MOVE 08               TO BVVEWE00-CODRET
                         MOVE 'TIPO DE COMUNICACAO INVALIDO'
                                               TO BVVEWE00-MSG
                         PERFORM 4000-FINALIZAR
                  END-EVALUATE
               WHEN 'BVVEWE20'
                  MOVE 15     TO WRK-COD-COMUNIC
               WHEN 'BVVEWE21'
                  EVALUATE BVVEWE21-TP-COMU
                      WHEN 1
                         IF BVVEWE21-IDENTD-OP EQUAL SPACES
                            MOVE 11     TO WRK-COD-COMUNIC
                         ELSE
                            MOVE 14     TO WRK-COD-COMUNIC
                         END-IF
                      WHEN 2
                         MOVE 16     TO WRK-COD-COMUNIC
                      WHEN 3
                         MOVE 17     TO WRK-COD-COMUNIC
                      WHEN OTHER
                         MOVE 08               TO BVVEWE00-CODRET
                         MOVE 'TIPO DE COMUNICACAO INVALIDO'
                                               TO BVVEWE00-MSG
                         PERFORM 4000-FINALIZAR
                  END-EVALUATE
               WHEN 'BVVEWE25'
                  MOVE 18     TO WRK-COD-COMUNIC
               WHEN 'BVVEWE27'
                  IF BVVEWE27-IDENTD-OP-CONTSTC EQUAL SPACES
                     MOVE 05     TO WRK-COD-COMUNIC
                  ELSE
                     MOVE 06     TO WRK-COD-COMUNIC
                  END-IF
               WHEN 'BVVEWX27'
                     MOVE 19     TO WRK-COD-COMUNIC
               WHEN OTHER
                  MOVE 08               TO BVVEWE00-CODRET
                  MOVE 'LEIAUTE DE COMUNICACAO INVALIDO'
                                        TO BVVEWE00-MSG
                  PERFORM 4000-FINALIZAR
           END-EVALUATE.

       1000-99-FIM. EXIT.

      *----------------------------------------------------------------*
      * RECUPERA AREA DA GAM PARA PROCESSAMENTO                        *
      *----------------------------------------------------------------*
       1050-RECUPERA-GAM               SECTION.

           INITIALIZE FRWKEL04-BLOCO-ENTRADA
                      FRWKEL04-BLOCO-RETORNO
                                  REPLACING NUMERIC BY ZEROS
                                       ALPHANUMERIC BY SPACES
      *
           SET  FRWKEL04-FUN-CONSULTAR TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKEL04-IDENT-AREA-GLOBAL
           MOVE BVVEWE00-COD-LAYOUT    TO FRWKEL04-ID-BLOCO
           MOVE BVVEWE00-TAM-LAYOUT    TO FRWKEL04-TAM-BLOCO
      *
           EXEC CICS LINK
                PROGRAM  (WRK-FRWK1200)
                COMMAREA (WRK-AREA-FRWK1200)
                LENGTH   (LENGTH OF WRK-AREA-FRWK1200)
                NOHANDLE
           END-EXEC
      *
           IF  (EIBRESP                NOT EQUAL DFHRESP(NORMAL))
              MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO BVVEWE00-MSG
              MOVE FRWKGLAQ-COD-RETORNO
                                       TO BVVEWE00-CODRET
              MOVE WRK-FRWK1500        TO FRWKW99B-MD-NOME-MODULO
              MOVE FRWKGLAQ-BLOCO-RETORNO
                                       TO FRWKW99B-MD-BLOCO-RETORNO
              MOVE '1050-RECUPERA-GAM' TO FRWKW99B-HE-IDEN-PARAGRAFO
              PERFORM 9500-ERRO-MODULO
           END-IF
      *
           IF (FRWKEL04-COD-RETORNO    NOT EQUAL ZEROS)
               MOVE 12                 TO FRWKGLAQ-COD-RETORNO
               MOVE '0000'             TO FRWKGLAQ-COD-ERRO
               MOVE FRWKEL04-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
               MOVE WRK-FRWK1200       TO FRWKGMOD-NOME-MODULO
               MOVE FRWKEL04-BLOCO-RETORNO
                                       TO FRWKGMOD-BLOCO-RETORNO
               MOVE '1050-RECUPERA-GAM' TO FRWKGHEA-IDEN-PARAGRAFO
               PERFORM 9500-ERRO-MODULO
           END-IF
           MOVE FRWKEL04-BUFFER(1:FRWKEL04-TAM-BLOCO)
                                       TO WRK-BVVEWE00.

           INSPECT WRK-BVVEWE00
                   REPLACING   ALL  LOW-VALUES     BY  SPACES.

       1050-99-FIM. EXIT.

      *----------------------------------------------------------------*
      * FORMATA A URL DA API A SER ACIONADA                            *
      *----------------------------------------------------------------*
       1100-FORMATA-URL                SECTION.

           EXEC CICS ASSIGN
                     APPLID (WRK-APPLID)
                     NOHANDLE
           END-EXEC.

           EVALUATE WRK-APPLID (6:3)
              WHEN 'M12'
                 MOVE WRK-PREFIX-PRD
                                       TO WRK-PARM1
              WHEN OTHER
                 MOVE WRK-PREFIX-HOM
                                       TO WRK-PARM1
           END-EVALUATE.

       1100-99-FIM. EXIT.

7C1121*----------------------------------------------------------------*
.     *    OBTER SERIE                                                 *
.     *----------------------------------------------------------------*
.      1200-ACESSAR-BVVEB057           SECTION.
.     *----------------------------------------------------------------*
.     *
.          MOVE 25                     TO CPARM-PROCM-AGNDA OF BVVEB057.
.     *
.          EXEC SQL
.              SELECT RCONTD_PARM_AGNDA
.              INTO  :BVVEB057.RCONTD-PARM-AGNDA
.              FROM   DB2PRD.TPARM_PROCM_AGNDA_CIPAG
.              WHERE  CPARM_PROCM_AGNDA = :BVVEB057.CPARM-PROCM-AGNDA
.              FETCH FIRST 1 ROWS ONLY
.          END-EXEC.
.     *
.          IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
.             (SQLWARN0                EQUAL 'W')
.              SET ERRO-DB2 OF WRK-AREA-ERRO
.                                      TO TRUE
.              MOVE 'SELECT'           TO FRWKGDB2-COMANDO-DB2
.              MOVE '1200'             TO FRWKGDB2-LOCAL
.              MOVE 'DB2PRD.TPARM_PROCM_AGNDA_CIPAG'
.                                      TO FRWKGDB2-NOME-TABELA
.              MOVE '1200-ACESSAR-BVVEB057'
.                                      TO FRWKGHEA-IDEN-PARAGRAFO
.              MOVE 16                 TO FRWKGLAQ-COD-RETORNO
.              MOVE WRK-PROGRAMA       TO FRWKGHEA-NOME-PROGRAMA
.              MOVE FRWKGDB2-TAM-LAYOUT
.                                      TO FRWKGHEA-TAM-DADOS
.              MOVE SQLSTATE           TO FRWKGDB2-SQLSTATE
.              MOVE SQLCA              TO FRWKGDB2-SQLCA
.              MOVE SQLCODE            TO FRWKGDB2-SQLCODE
.              MOVE WRK-ERRO-DB2       TO WRK-BLOCO-INFO-ERRO
.                                         (1:FRWKGHEA-TAM-DADOS)
.              PERFORM 9999-API-ERROS
.          END-IF
.     *
.          IF SQLCODE                  EQUAL +100
.             MOVE SPACES              TO WRK-SERIE
.          ELSE
.             MOVE RCONTD-PARM-AGNDA (1:32)
.                                      TO WRK-SERIE
.          END-IF.
.     *
.     *----------------------------------------------------------------*
.      1200-99-FIM.                    EXIT.
7C1121*----------------------------------------------------------------*
.     *
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA.                        *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.

           PERFORM 2010-CRIA-HEADER-REQ.

           PERFORM 2020-GERA-HEADER-REQ.

           PERFORM 2050-CRIAR-JSON.

           PERFORM 2002-RECUP-JSON.

           PERFORM 2060-CRIAR-BLOCO-EEXT.

           PERFORM 2300-CONEXAO-EEXT.

           IF EEXTW301-COD-STATUS EQUAL 200
              MOVE ZEROS                  TO BVVEWE00-CODRET
           ELSE
              MOVE 08                     TO BVVEWE00-CODRET
MB0501        MOVE EEXTW301-COD-STATUS    TO WRK-ERRO-EEXT-NUM
MB0501        STRING 'ERRO ' WRK-ERRO-EEXT
                     ' NO ACESSO AO BACK  '
              DELIMITED BY SIZE INTO BVVEWE00-MSG
           END-IF.
           INITIALIZE  WRK-BVVE3519-COMMAREA
           MOVE 0                      TO BVVEWM20-STATUS-CONV
           MOVE SPACES                 TO BVVEWM20-MSGERR-CONV
           MOVE 0                      TO BVVEWM20-STATUS-OPEN
           MOVE SPACES                 TO BVVEWM20-MSGERR-OPEN
           PERFORM 2003-GRAVA-MONITOR.

       2000-99-FIM. EXIT.

      *---------------------------------------------------------------*
      * RECUPERA OBJETO-JSON-PRE-FORMALIZAÇÃO SEM RESERVA OK          *
      *---------------------------------------------------------------*
       2001-LOGA-HEADER                SECTION.

      *
              MOVE '2001-LOGA-ENTRADA'    TO FRWKGHEA-IDEN-PARAGRAFO

              SET  ERRO-LIVRE OF WRK-AREA-ERRO       TO TRUE
              MOVE 'BVVE1CIP'          TO FRWKGHEA-NOME-PROGRAMA
              MOVE 1                   TO FRWKGLIV-IDIOMA
              MOVE 'FEMP3026'          TO FRWKGLIV-COD-MENSAGEM
              MOVE WRK-BVVEWE00        TO FRWKGLIV-PARAMETROS
              MOVE FRWKGLIV-TAM-LAYOUT TO FRWKGHEA-TAM-DADOS
              MOVE WRK-AREA-ERRO-LIVRE TO WRK-BLOCO-INFO-ERRO
              EXEC CICS LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
              END-EXEC

              IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
                 CONTINUE
              END-IF.

              MOVE '2001-LOGA-HEADER'     TO FRWKGHEA-IDEN-PARAGRAFO

              SET  ERRO-LIVRE OF WRK-AREA-ERRO       TO TRUE
              MOVE 'BVVE1CIP'          TO FRWKGHEA-NOME-PROGRAMA
              MOVE 1                   TO FRWKGLIV-IDIOMA
              MOVE 'FEMP3026'          TO FRWKGLIV-COD-MENSAGEM
              MOVE EEXTWBLH-HEADERS    TO FRWKGLIV-PARAMETROS
              MOVE FRWKGLIV-TAM-LAYOUT TO FRWKGHEA-TAM-DADOS
              MOVE WRK-AREA-ERRO-LIVRE TO WRK-BLOCO-INFO-ERRO
              EXEC CICS LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
              END-EXEC

              IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
                 CONTINUE
              END-IF.

       2001-99-FIM. EXIT.
      *---------------------------------------------------------------*
      * RECUPERA OBJETO-JSON-PRE-FORMALIZAÇÃO SEM RESERVA OK          *
      *---------------------------------------------------------------*
       2002-RECUP-JSON                 SECTION.

      *--- RECUPERA-OBJETO-JSON

           INITIALIZE  FRWKW203-BLOCO-ENTRADA
                       FRWKW203-BLOCO-RETORNO
                       FRWKW203-BLOCO-ENTRADA-SAIDA
                       WRK-GLOG.

           SET FRWKW203-FUN-CONSULTAR  TO TRUE.
           SET FRWKW203-PERM-CONSULTAR TO TRUE.
           SET FRWKW203-IND-BL-SAIDA-JS
                                       TO TRUE.

           MOVE SPACES                 TO WRK-GLOG.
           MOVE SPACES                 TO FRWKW203-IDENT-AREA-GLOBAL.
           MOVE 'BVVEWENT'             TO FRWKW203-ID-BLOCO.
           MOVE LENGTH OF WRK-GLOG
                                       TO FRWKW203-TAM-BLOCO.

           SET FRWKW203-PTR-AREA1      TO ADDRESS
                                       OF WRK-GLOG

           MOVE LENGTH OF WRK-GLOG
                                        TO FRWKW203-TAM-AREA1.

           SET  FRWKW203-PTR-AREA2       TO NULL
           MOVE ZEROS                    TO FRWKW203-TAM-AREA2

           EXEC CICS LINK
                     PROGRAM  (WRK-FRWK1200)
                     COMMAREA (WRK-FRWKW203)
                     LENGTH   (LENGTH OF WRK-FRWKW203)
                     NOHANDLE
           END-EXEC.

           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
              GO TO 2002-99-FIM
           END-IF.

           IF FRWKW203-COD-RETORNO EQUAL ZEROS
      *
              MOVE '2002-RECUP-JSON'      TO FRWKGHEA-IDEN-PARAGRAFO
              SET  ERRO-LIVRE OF WRK-AREA-ERRO       TO TRUE
              MOVE 'BVVE1CIP'          TO FRWKGHEA-NOME-PROGRAMA
              MOVE 1                   TO FRWKGLIV-IDIOMA
              MOVE 'FEMP3026'          TO FRWKGLIV-COD-MENSAGEM
              MOVE WRK-GLOG            TO FRWKGLIV-PARAMETROS
              MOVE FRWKGLIV-TAM-LAYOUT TO FRWKGHEA-TAM-DADOS
              MOVE WRK-AREA-ERRO-LIVRE TO WRK-BLOCO-INFO-ERRO
              EXEC CICS LINK
                PROGRAM  (WRK-FRWK1999)
                COMMAREA (WRK-AREA-ERRO)
                LENGTH   (LENGTH OF WRK-AREA-ERRO)
                NOHANDLE
              END-EXEC

              IF  EIBRESP                 NOT EQUAL DFHRESP(NORMAL)
                 CONTINUE
              END-IF
           END-IF.

           INITIALIZE  FRWKW203-BLOCO-ENTRADA
                       FRWKW203-BLOCO-RETORNO
                       FRWKW203-BLOCO-ENTRADA-SAIDA.

           SET  FRWKW203-FUN-LIBERAR     TO TRUE
           SET  FRWKW203-PERM-ATUALIZAR  TO TRUE
           SET  FRWKW203-IND-BL-OUTRO-JS  TO TRUE
           MOVE ZEROS         TO WRK-TAM-JSON
           MOVE ZEROS         TO WRK-TAM-COBOL
           COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                      WRK-TAM-COBOL
           SET  FRWKW203-PTR-AREA1     TO NULL
           MOVE ZEROS                  TO FRWKW203-TAM-AREA1
           SET  FRWKW203-PTR-AREA2     TO NULL
           MOVE ZEROS                  TO FRWKW203-TAM-AREA2
           MOVE SPACES                 TO FRWKW203-IDENT-AREA-GLOBAL
           MOVE 'BVVEWENT'             TO FRWKW203-ID-BLOCO
           MOVE WRK-SOMA-TAMANHO       TO FRWKW203-TAM-BLOCO



           PERFORM  8000-ACIONAR-FRWK1200.

           INITIALIZE  FRWKW203-BLOCO-ENTRADA
                       FRWKW203-BLOCO-RETORNO
                       FRWKW203-BLOCO-ENTRADA-SAIDA.

           INSPECT WRK-GLOG
                   REPLACING   ALL     'Ý'         BY  '['.
           INSPECT WRK-GLOG
                   REPLACING   ALL     '¨'         BY  ']'.

           SET  FRWKW203-FUN-CRIAR        TO TRUE
           SET  FRWKW203-PERM-ATUALIZAR   TO TRUE
           SET  FRWKW203-IND-BL-OUTRO-RQS TO TRUE
           MOVE ZEROS         TO WRK-TAM-JSON
           MOVE LENGTH OF WRK-GLOG
                              TO WRK-TAM-COBOL
           COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                      WRK-TAM-COBOL
           SET  FRWKW203-PTR-AREA1     TO
                               ADDRESS OF WRK-GLOG
           MOVE LENGTH OF WRK-GLOG
                               TO FRWKW203-TAM-AREA1
           SET  FRWKW203-PTR-AREA2     TO NULL
           MOVE ZEROS                  TO FRWKW203-TAM-AREA2
           MOVE SPACES                 TO FRWKW203-IDENT-AREA-GLOBAL
           MOVE 'BVVEWENT'             TO FRWKW203-ID-BLOCO
           MOVE WRK-SOMA-TAMANHO       TO FRWKW203-TAM-BLOCO

           PERFORM  8000-ACIONAR-FRWK1200.

       2002-99-FIM. EXIT.
      *---------------------------------------------------------------*
      * GERA REGISTRO NO MONITOR                                      *
      *---------------------------------------------------------------*
       2003-GRAVA-MONITOR              SECTION.

      *--- RECUPERA-OBJETO-JSON


           MOVE LENGTH OF BVVEWE00-MENSAGEM
                                       TO BVVEWM20-TAM-POSIC
           MOVE LENGTH OF WRK-MONITOR  TO BVVEWM20-TAM-JSON
           MOVE BVVEWE00-LEIAUTE       TO BVVEWM20-CODMSG
           MOVE SPACES                 TO BVVEWM20-NUM-UNICO-SIST
           MOVE SPACES                 TO BVVEWM20-NUOP
           MOVE BVVEWE00-ID            TO BVVEWM20-CTRLIF
           MOVE '60746948'             TO BVVEWM20-CEMISR-MSGEM
           MOVE '29011780'             TO BVVEWM20-CDSTNA-MSGEM
           MOVE SPACES                 TO BVVEWM20-HOCORR-WKFLW
           MOVE 'CIP'                  TO BVVEWM20-CDOM-MSGRA-SPB
           MOVE 'N'                    TO BVVEWM20-ENTRADA-MANUAL
           MOVE 'UAPCIPB'              TO BVVEWM20-USUARIO
           MOVE BVVEWE00-MENSAGEM(1:10244)
                                       TO BVVEWM22-POSICIONAL
           MOVE WRK-MONITOR            TO BVVEWM22-JSON

           EXEC CICS
               START
               TRANSID ('VVKA')
               FROM (WRK-BVVE3519-COMMAREA)
               LENGTH  (LENGTH OF WRK-BVVE3519-COMMAREA)
               NOHANDLE
           END-EXEC.

           IF (EIBRESP                 NOT EQUAL DFHRESP(NORMAL))
              GO TO 2003-99-FIM
           END-IF.


       2003-99-FIM. EXIT.
      *---------------------------------------------------------------*
      * POPULA HEADER PARA CHAMADA A CIP                              *
      *---------------------------------------------------------------*
       2010-CRIA-HEADER-REQ            SECTION.

           MOVE SPACES TO WRK-PARM2
                          WRK-PARM3
                          WRK-PARM4
                          WRK-PARM5
                          WRK-PARM6
                          WRK-PARM7
                          WRK-PARM8
                          WRK-PARM9
                          WRK-PARM10
                          WRK-PARM11
                          WRK-PARM12
                          WRK-PARM13
                          WRK-PARM14
                          WRK-PARM15
                          WRK-PARM16
                          WRK-PARM17
                          WRK-PARM18
                          WRK-PARM19
                          WRK-PARM20
                          WRK-PARM21
                          WRK-PARM22
                          WRK-PARM23
                          WRK-PARM24
TON01                     WRK-PARM25
TON01                     WRK-PARM26.

           EVALUATE WRK-COD-COMUNIC
               WHEN 1
                  MOVE WRK-PATH-E01          TO WRK-PARM2
                  MOVE '?msg='               TO WRK-PARM3
                  MOVE BVVEWE01-MSG          TO WRK-PARM4
               WHEN 2
                  MOVE SPACES                TO WRK-PRIM-QUERY
                  MOVE 1                     TO BVVEWE10-NUMEROPAGINA
                  MOVE 1000                  TO BVVEWE10-TAMANHOPAGINA
                  MOVE WRK-PATH-E02          TO WRK-PARM2
                  IF BVVEWE10-CNPJOUCPFUSUFINRECBDR NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?cnpjOuCnpjBaseOuCpfUsuFinalRecbdr='
                                                          TO WRK-PARM3
                        MOVE BVVEWE10-CNPJOUCPFUSUFINRECBDR
                                                          TO WRK-PARM4
                     ELSE
                        MOVE '&cnpjOuCnpjBaseOuCpfUsuFinalRecbdr='
                                                          TO WRK-PARM3
                        MOVE BVVEWE10-CNPJOUCPFUSUFINRECBDR
                                                          TO WRK-PARM4
                     END-IF
                  END-IF
                  IF BVVEWE10-CODINSTITDRARRAJPGTO NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?codInstitdrArrajPgto='
                                                          TO WRK-PARM5
                        MOVE BVVEWE10-CODINSTITDRARRAJPGTO
                                                          TO WRK-PARM6
                     ELSE
                        MOVE '&codInstitdrArrajPgto='
                                                          TO WRK-PARM5
                        MOVE BVVEWE10-CODINSTITDRARRAJPGTO
                                                          TO WRK-PARM6
                     END-IF
                  END-IF
                  IF BVVEWE10-CNPJCREDDRSUB NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?cnpjCreddrSub='
                                                          TO WRK-PARM7
                        MOVE BVVEWE10-CNPJCREDDRSUB
                                                          TO WRK-PARM8
                     ELSE
                        MOVE '&cnpjCreddrSub='
                                                          TO WRK-PARM7
                        MOVE BVVEWE10-CNPJCREDDRSUB
                                                          TO WRK-PARM8
                     END-IF
                  END-IF
                  IF BVVEWE10-DTPREVTLIQUID NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?dtPrevtLiquid='
                                                          TO WRK-PARM9
                        MOVE BVVEWE10-DTPREVTLIQUID
                                                          TO WRK-PARM10
                     ELSE
                        MOVE '&dtPrevtLiquid='
                                                          TO WRK-PARM9
                        MOVE BVVEWE10-DTPREVTLIQUID
                                                          TO WRK-PARM10
                     END-IF
                  END-IF
                  IF BVVEWE10-VLRLIVRE NOT EQUAL ZEROS
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?vlrLivre='
                                                          TO WRK-PARM11
                        MOVE BVVEWE10-VLRLIVRE
                                                        TO WRK-PARM12-9
                     ELSE
                        MOVE '&vlrLivre='
                                                          TO WRK-PARM11
                        MOVE BVVEWE10-VLRLIVRE
                                                        TO WRK-PARM12-9
                     END-IF
                  END-IF
                  IF BVVEWE10-DTINIPREVTLIQUID NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?dtIniPrevtLiquid='
                                                          TO WRK-PARM13
                        MOVE BVVEWE10-DTINIPREVTLIQUID
                                                          TO WRK-PARM14
                     ELSE
                        MOVE '&dtIniPrevtLiquid='
                                                          TO WRK-PARM13
                        MOVE BVVEWE10-DTINIPREVTLIQUID
                                                          TO WRK-PARM14
                     END-IF
                  END-IF
                  IF BVVEWE10-DTFIMPREVTLIQUID NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?dtFimPrevtLiquid='
                                                          TO WRK-PARM15
                        MOVE BVVEWE10-DTFIMPREVTLIQUID
                                                          TO WRK-PARM16
                     ELSE
                        MOVE '&dtFimPrevtLiquid='
                                                          TO WRK-PARM15
                        MOVE BVVEWE10-DTFIMPREVTLIQUID
                                                          TO WRK-PARM16
                     END-IF
                  END-IF
                  IF BVVEWE10-INDRTPNEGC NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?indrTpNegc='
                                                          TO WRK-PARM17
                        MOVE BVVEWE10-INDRTPNEGC
                                                          TO WRK-PARM18
                     ELSE
                        MOVE '&indrTpNegc='
                                                          TO WRK-PARM17
                        MOVE BVVEWE10-INDRTPNEGC
                                                          TO WRK-PARM18
                     END-IF
                  END-IF
                  IF BVVEWE10-CNPJCPFTITLAR NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?cnpjOuCnpjBaseOuCpfTitlar='
                                                          TO WRK-PARM19
                        MOVE BVVEWE10-CNPJCPFTITLAR
                                                          TO WRK-PARM20
                     ELSE
                        MOVE '&cnpjOuCnpjBaseOuCpfTitlar='
                                                          TO WRK-PARM19
                        MOVE BVVEWE10-CNPJCPFTITLAR
                                                          TO WRK-PARM20
                     END-IF
                  END-IF
                  IF BVVEWE10-NUMEROPAGINA NOT EQUAL ZEROS
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?numeroPagina='
                                                          TO WRK-PARM21
                        MOVE BVVEWE10-NUMEROPAGINA-X
                                                          TO WRK-PARM22
                     ELSE
                        MOVE '&numeroPagina='
                                                          TO WRK-PARM21
                        MOVE BVVEWE10-NUMEROPAGINA-X
                                                          TO WRK-PARM22
                     END-IF
                  END-IF
                  IF BVVEWE10-TAMANHOPAGINA NOT EQUAL ZEROS
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?tamanhoPagina='
                                                          TO WRK-PARM23
                        MOVE BVVEWE10-TAMANHOPAGINA-X
                                                          TO WRK-PARM24
                     ELSE
                        MOVE '&tamanhoPagina='
                                                          TO WRK-PARM23
                        MOVE BVVEWE10-TAMANHOPAGINA-X
                                                          TO WRK-PARM24
                     END-IF
                  END-IF
TON01             IF BVVEWE10-INTEROPERABILIDADE NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?IndrInterop='
                                                          TO WRK-PARM25
                        MOVE BVVEWE10-INTEROPERABILIDADE
                                                          TO WRK-PARM26
                     ELSE
                        MOVE '&IndrInterop='
                                                          TO WRK-PARM25
                        MOVE BVVEWE10-INTEROPERABILIDADE
                                                          TO WRK-PARM26
                     END-IF
TON01             END-IF
               WHEN 3
                  MOVE WRK-PATH-E03          TO WRK-PARM2
               WHEN 4
                  MOVE BVVEWE13-IDENTD-CTRL-OPT-IN
                                             TO WRK-PATH-E04-ID
                  MOVE WRK-PATH-E04          TO WRK-PARM2
                  MOVE '?identdCtrlReqSolicte='
                                             TO WRK-PARM3
                  MOVE BVVEWE13-IDENTDCTRLREQSOLICTE
                                             TO WRK-PARM4
               WHEN 5
                  MOVE WRK-PATH-E05          TO WRK-PARM2
               WHEN 6
                  MOVE BVVEWE27-IDENTD-OP-CONTSTC
                                             TO WRK-PATH-E06-ID
                  MOVE WRK-PATH-E06          TO WRK-PARM2
               WHEN 7
                  MOVE WRK-PATH-E07          TO WRK-PARM2
               WHEN 8
                  MOVE BVVEWE19-IDENTDCONJUNIRECBVL-P
                                             TO WRK-PATH-E08-ID
                  MOVE WRK-PATH-E08          TO WRK-PARM2
               WHEN 9
                  MOVE WRK-PATH-E09          TO WRK-PARM2
               WHEN 10
                  MOVE WRK-PATH-E10          TO WRK-PARM2
               WHEN 11
                  MOVE WRK-PATH-E11          TO WRK-PARM2
                  MOVE SPACES                TO WRK-PRIM-QUERY
                  MOVE 1                     TO BVVEWE21-NUMEROPAGINA
                  MOVE 50                    TO BVVEWE21-TAMANHOPAGINA
                  IF BVVEWE21-CNPJCPFUSUFINRECBDR NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?cnpjOuCnpjBaseOuCpfUsuFinalRecbdr=    '
                                                          TO WRK-PARM3
                        MOVE BVVEWE21-CNPJCPFUSUFINRECBDR
                                                          TO WRK-PARM4
                     ELSE
                        MOVE '&cnpjOuCnpjBaseOuCpfUsuFinalRecbdr=    '
                                                          TO WRK-PARM3
                        MOVE BVVEWE21-CNPJCPFUSUFINRECBDR
                                                          TO WRK-PARM4
                     END-IF
                  END-IF
                  IF BVVEWE21-CNPJCPFTITLAR       NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?cnpjOuCnpjBaseOuCpfTitlar=            '
                                                          TO WRK-PARM5
                        MOVE BVVEWE21-CNPJCPFTITLAR
                                                          TO WRK-PARM6
                     ELSE
                        MOVE '&cnpjOuCnpjBaseOuCpfTitlar=            '
                                                          TO WRK-PARM5
                        MOVE BVVEWE21-CNPJCPFTITLAR
                                                          TO WRK-PARM6
                     END-IF
                  END-IF
                  IF BVVEWE21-NUMEROPAGINA-X      NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?numeroPagina=                         '
                                                          TO WRK-PARM7
                        MOVE BVVEWE21-NUMEROPAGINA-X
                                                          TO WRK-PARM8
                     ELSE
                        MOVE '&numeroPagina=                         '
                                                          TO WRK-PARM7
                        MOVE BVVEWE21-NUMEROPAGINA-X
                                                          TO WRK-PARM8
                     END-IF
                  END-IF
                  IF BVVEWE21-TAMANHOPAGINA-X     NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?tamanhoPagina=                        '
                                                          TO WRK-PARM9
                        MOVE BVVEWE21-TAMANHOPAGINA-X
                                                          TO WRK-PARM10
                     ELSE
                        MOVE '&tamanhoPagina=                        '
                                                          TO WRK-PARM9
                        MOVE BVVEWE21-TAMANHOPAGINA-X
                                                          TO WRK-PARM10
                     END-IF
                  END-IF
               WHEN 12
                  MOVE BVVEWE19-IDENTD-OP-P
                                             TO WRK-PATH-E12-ID
                  MOVE WRK-PATH-E12          TO WRK-PARM2
               WHEN 13
                  MOVE BVVEWE19-IDENTD-OP-P
                                             TO WRK-PATH-E13-ID
                  MOVE WRK-PATH-E13          TO WRK-PARM2
               WHEN 14
                  MOVE BVVEWE21-IDENTD-OP
                                             TO WRK-PATH-E14-ID
                  MOVE WRK-PATH-E14          TO WRK-PARM2
               WHEN 15
                  MOVE BVVEWE20-IDENTD-OP
                                             TO WRK-PATH-E15-ID
                  MOVE WRK-PATH-E15          TO WRK-PARM2
               WHEN 16
                  MOVE BVVEWE21-IDENTD-OP
                                             TO WRK-PATH-E16-ID
                  MOVE WRK-PATH-E16          TO WRK-PARM2
                  MOVE SPACES                TO WRK-PRIM-QUERY
                  MOVE 1                     TO BVVEWE21-NUMEROPAGINA
                  MOVE 50                    TO BVVEWE21-TAMANHOPAGINA
                  IF BVVEWE21-NUMEROPAGINA-X      NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?numeroPagina=                         '
                                                          TO WRK-PARM7
                        MOVE BVVEWE21-NUMEROPAGINA-X
                                                          TO WRK-PARM8
                     ELSE
                        MOVE '&numeroPagina=                         '
                                                          TO WRK-PARM7
                        MOVE BVVEWE21-NUMEROPAGINA-X
                                                          TO WRK-PARM8
                     END-IF
                  END-IF
                  IF BVVEWE21-TAMANHOPAGINA-X     NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?tamanhoPagina=                        '
                                                          TO WRK-PARM9
                        MOVE BVVEWE21-TAMANHOPAGINA-X
                                                          TO WRK-PARM10
                     ELSE
                        MOVE '&tamanhoPagina=                        '
                                                          TO WRK-PARM9
                        MOVE BVVEWE21-TAMANHOPAGINA-X
                                                          TO WRK-PARM10
                     END-IF
                  END-IF
               WHEN 17
                  MOVE BVVEWE21-IDENTD-OP
                                             TO WRK-PATH-E17-ID
                  MOVE WRK-PATH-E17          TO WRK-PARM2
                  MOVE SPACES                TO WRK-PRIM-QUERY
                  MOVE 1                     TO BVVEWE21-NUMEROPAGINA
                  MOVE 50                    TO BVVEWE21-TAMANHOPAGINA
                  IF BVVEWE21-NUMEROPAGINA-X      NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?numeroPagina=                         '
                                                          TO WRK-PARM7
                        MOVE BVVEWE21-NUMEROPAGINA-X
                                                          TO WRK-PARM8
                     ELSE
                        MOVE '&numeroPagina=                         '
                                                          TO WRK-PARM7
                        MOVE BVVEWE21-NUMEROPAGINA-X
                                                          TO WRK-PARM8
                     END-IF
                  END-IF
                  IF BVVEWE21-TAMANHOPAGINA-X     NOT EQUAL SPACES
                     IF WRK-PRIM-QUERY EQUAL SPACES
                        MOVE '*' TO WRK-PRIM-QUERY
                        MOVE '?tamanhoPagina=                        '
                                                          TO WRK-PARM9
                        MOVE BVVEWE21-TAMANHOPAGINA-X
                                                          TO WRK-PARM10
                     ELSE
                        MOVE '&tamanhoPagina=                        '
                                                          TO WRK-PARM9
                        MOVE BVVEWE21-TAMANHOPAGINA-X
                                                          TO WRK-PARM10
                     END-IF
                  END-IF
               WHEN 18
                  MOVE BVVEWE25-IDENTD-OP
                                             TO WRK-PATH-E18-ID
                  MOVE WRK-PATH-E18          TO WRK-PARM2
           END-EVALUATE.

           STRING         WRK-PARM1
                 DELIMITED BY SPACES INTO WRK-URL-AUX
             END-STRING.

           STRING         WRK-PARM2
                          WRK-PARM3
                          WRK-PARM4
                          WRK-PARM5
                          WRK-PARM6
                          WRK-PARM7
                          WRK-PARM8
                          WRK-PARM9
                          WRK-PARM10
                          WRK-PARM11
                          WRK-PARM12
                          WRK-PARM13
                          WRK-PARM14
                          WRK-PARM15
                          WRK-PARM16
                          WRK-PARM17
                          WRK-PARM18
                          WRK-PARM19
                          WRK-PARM20
                          WRK-PARM21
                          WRK-PARM22
                          WRK-PARM23
                          WRK-PARM24
                          WRK-PARM25
                          WRK-PARM26
                 DELIMITED BY SPACES INTO WRK-QS-AUX
             END-STRING.

           MOVE '/retornos-registro-operacao' TO WRK-PARM99

           STRING         WRK-PARM1
                          WRK-PARM99
                 DELIMITED BY SPACES INTO WRK-URL-CALLBACK
             END-STRING.

           EVALUATE WRK-COD-COMUNIC
               WHEN 1
                  MOVE WRK-URL-AUX           TO WRK-URL-E01
                  MOVE WRK-QS-AUX            TO WRK-QS-E01
                  MOVE BVVEWE00-ID           TO  WRK-ID-E01
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E01
               WHEN 2
                  MOVE WRK-URL-AUX           TO WRK-URL-E02
                  MOVE WRK-QS-AUX            TO WRK-QS-E02
                  MOVE BVVEWE00-ID           TO  WRK-ID-E02
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E02
               WHEN 3
                  MOVE WRK-URL-AUX           TO WRK-URL-E03
                  MOVE WRK-QS-AUX            TO WRK-QS-E03
                  MOVE BVVEWE00-ID           TO  WRK-ID-E03
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E03
               WHEN 4
                  MOVE WRK-URL-AUX           TO WRK-URL-E04
                  MOVE WRK-QS-AUX            TO WRK-QS-E04
                  MOVE BVVEWE00-ID           TO  WRK-ID-E04
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E04
               WHEN 5
                  MOVE WRK-URL-AUX           TO WRK-URL-E05
                  MOVE WRK-QS-AUX            TO WRK-QS-E05
                  MOVE BVVEWE00-ID           TO  WRK-ID-E05
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E05
               WHEN 6
                  MOVE WRK-URL-AUX           TO WRK-URL-E06
                  MOVE WRK-QS-AUX            TO WRK-QS-E06
                  MOVE BVVEWE00-ID           TO  WRK-ID-E06
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E06
               WHEN 7
                  MOVE WRK-URL-AUX           TO WRK-URL-E07
                  MOVE WRK-QS-AUX            TO WRK-QS-E07
                  MOVE BVVEWE00-ID           TO  WRK-ID-E07
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E07
               WHEN 8
                  MOVE WRK-URL-AUX           TO WRK-URL-E08
                  MOVE WRK-QS-AUX            TO WRK-QS-E08
                  MOVE BVVEWE00-ID           TO  WRK-ID-E08
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E08
               WHEN 9
                  MOVE WRK-URL-AUX           TO WRK-URL-E09
                  MOVE WRK-QS-AUX            TO WRK-QS-E09
                  MOVE BVVEWE00-ID           TO  WRK-ID-E09
                  MOVE WRK-URL-CALLBACK      TO WRK-URL-CB-09
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E09
               WHEN 10
                  MOVE WRK-URL-AUX           TO WRK-URL-E10
                  MOVE WRK-QS-AUX            TO WRK-QS-E10
                  MOVE BVVEWE00-ID           TO  WRK-ID-E10
                  MOVE WRK-URL-CALLBACK      TO WRK-URL-CB-10
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E10
               WHEN 11
                  MOVE WRK-URL-AUX           TO WRK-URL-E11
                  MOVE WRK-QS-AUX            TO WRK-QS-E11
                  MOVE BVVEWE00-ID           TO  WRK-ID-E11
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E11
               WHEN 12
                  MOVE WRK-URL-AUX           TO WRK-URL-E12
                  MOVE WRK-QS-AUX            TO WRK-QS-E12
                  MOVE BVVEWE00-ID           TO  WRK-ID-E12
                  MOVE WRK-URL-CALLBACK      TO WRK-URL-CB-12
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E12
               WHEN 13
                  MOVE WRK-URL-AUX           TO WRK-URL-E13
                  MOVE WRK-QS-AUX            TO WRK-QS-E13
                  MOVE BVVEWE00-ID           TO  WRK-ID-E13
                  MOVE WRK-URL-CALLBACK      TO WRK-URL-CB-13
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E13
               WHEN 14
                  MOVE WRK-URL-AUX           TO WRK-URL-E14
                  MOVE WRK-QS-AUX            TO WRK-QS-E14
                  MOVE BVVEWE00-ID           TO  WRK-ID-E14
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E14
               WHEN 15
                  MOVE WRK-URL-AUX           TO WRK-URL-E15
                  MOVE WRK-QS-AUX            TO WRK-QS-E15
                  MOVE BVVEWE00-ID           TO  WRK-ID-E15
                  MOVE WRK-URL-CALLBACK      TO WRK-URL-CB-15
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E15
               WHEN 16
                  MOVE WRK-URL-AUX           TO WRK-URL-E16
                  MOVE WRK-QS-AUX            TO WRK-QS-E16
                  MOVE BVVEWE00-ID           TO  WRK-ID-E16
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E16
               WHEN 17
                  MOVE WRK-URL-AUX           TO WRK-URL-E17
                  MOVE WRK-QS-AUX            TO WRK-QS-E17
                  MOVE BVVEWE00-ID           TO  WRK-ID-E17
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E17
               WHEN 18
                  MOVE WRK-URL-AUX           TO WRK-URL-E18
                  MOVE WRK-QS-AUX            TO WRK-QS-E18
                  MOVE BVVEWE00-ID           TO  WRK-ID-E18
7C1121            MOVE WRK-SERIE             TO  WRK-SE-E18
           END-EVALUATE.

       2010-99-FIM. EXIT.

      *---------------------------------------------------------------*
       2020-GERA-HEADER-REQ            SECTION.

           SET  EEXTWBLH-VERSAO-001    TO TRUE.
           MOVE 13                     TO EEXTWBLH-QTD-HEADERS.

           EVALUATE WRK-COD-COMUNIC
               WHEN 1
                 MOVE BVVEWMAP-E01-HEADER    TO EEXTWBLH-HEADERS
               WHEN 2
                 MOVE BVVEWMAP-E02-HEADER    TO EEXTWBLH-HEADERS
               WHEN 3
                 MOVE BVVEWMAP-E03-HEADER    TO EEXTWBLH-HEADERS
               WHEN 4
                 MOVE BVVEWMAP-E04-HEADER    TO EEXTWBLH-HEADERS
               WHEN 5
                 MOVE BVVEWMAP-E05-HEADER    TO EEXTWBLH-HEADERS
               WHEN 6
                 MOVE BVVEWMAP-E06-HEADER    TO EEXTWBLH-HEADERS
               WHEN 7
                 MOVE BVVEWMAP-E07-HEADER    TO EEXTWBLH-HEADERS
               WHEN 8
                 MOVE BVVEWMAP-E08-HEADER    TO EEXTWBLH-HEADERS
               WHEN 9
                 MOVE BVVEWMAP-E09-HEADER    TO EEXTWBLH-HEADERS
               WHEN 10
                 MOVE BVVEWMAP-E10-HEADER    TO EEXTWBLH-HEADERS
               WHEN 11
                 MOVE BVVEWMAP-E11-HEADER    TO EEXTWBLH-HEADERS
               WHEN 12
                 MOVE BVVEWMAP-E12-HEADER    TO EEXTWBLH-HEADERS
               WHEN 13
                 MOVE BVVEWMAP-E13-HEADER    TO EEXTWBLH-HEADERS
               WHEN 14
                 MOVE BVVEWMAP-E14-HEADER    TO EEXTWBLH-HEADERS
               WHEN 15
                 MOVE BVVEWMAP-E15-HEADER    TO EEXTWBLH-HEADERS
               WHEN 16
                 MOVE BVVEWMAP-E16-HEADER    TO EEXTWBLH-HEADERS
               WHEN 17
                 MOVE BVVEWMAP-E17-HEADER    TO EEXTWBLH-HEADERS
               WHEN 18
                 MOVE BVVEWMAP-E18-HEADER    TO EEXTWBLH-HEADERS
           END-EVALUATE.

           INITIALIZE  FRWKW203-BLOCO-ENTRADA
                       FRWKW203-BLOCO-RETORNO
                       FRWKW203-BLOCO-ENTRADA-SAIDA

           SET  FRWKW203-FUN-CRIAR       TO TRUE
           SET  FRWKW203-PERM-ATUALIZAR  TO TRUE
           SET  FRWKW203-IND-BL-OUTRO    TO TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                         TO FRWKW203-IDENT-AREA-GLOBAL
           MOVE EEXTWBLH-COD-LAYOUT      TO FRWKW203-ID-BLOCO
           MOVE EEXTWBLH-TAM-LAYOUT      TO FRWKW203-TAM-BLOCO
           SET  FRWKW203-PTR-AREA1       TO ADDRESS OF WRK-EEXTWBLH
           MOVE LENGTH OF WRK-EEXTWBLH   TO FRWKW203-TAM-AREA1
           SET  FRWKW203-PTR-AREA2       TO NULL
           MOVE ZEROS                    TO FRWKW203-TAM-AREA2

           PERFORM 8000-ACIONAR-FRWK1200.

           PERFORM 2001-LOGA-HEADER.

       2020-99-FIM. EXIT.

      *----------------------------------------------------------------*
       2050-CRIAR-JSON                 SECTION.

           INITIALIZE  FRWKW203-BLOCO-ENTRADA
                       FRWKW203-BLOCO-RETORNO
                       FRWKW203-BLOCO-ENTRADA-SAIDA.

           SET FRWKW203-FUN-CRIAR      TO TRUE.
           SET FRWKW203-PERM-CONSULTAR TO TRUE.
           SET FRWKW203-IND-BL-OUTRO-JS
                                       TO TRUE.

           EVALUATE WRK-COD-COMUNIC
               WHEN 1
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E01-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE01-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE01-MAPA
                    MOVE LENGTH OF BVVEWE01-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E01-BODY
                    MOVE LENGTH OF BVVEWMAP-E01-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 2
               WHEN 4
               WHEN 7
               WHEN 11
               WHEN 14
               WHEN 16
               WHEN 17
                    SET FRWKW203-IND-BL-OUTRO-RQS TO TRUE
                    MOVE ZEROS         TO WRK-TAM-JSON
                    MOVE LENGTH OF WRK-VAZIO
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF WRK-VAZIO
                    MOVE LENGTH OF WRK-VAZIO
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO NULL
                    MOVE ZEROS                  TO FRWKW203-TAM-AREA2
               WHEN 3
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E03-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE11-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE11-MAPA
                    MOVE LENGTH OF BVVEWE11-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E03-BODY
                    MOVE LENGTH OF BVVEWMAP-E03-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 5
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E05-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE27-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE27-MAPA
                    MOVE LENGTH OF BVVEWE27-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E05-BODY
                    MOVE LENGTH OF BVVEWMAP-E05-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 19
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E19-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWX27-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWX27-MAPA
                    MOVE LENGTH OF BVVEWX27-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E19-BODY
                    MOVE LENGTH OF BVVEWMAP-E19-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 6
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E06-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE27-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE27-MAPA
                    MOVE LENGTH OF BVVEWE27-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E06-BODY
                    MOVE LENGTH OF BVVEWMAP-E06-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 8
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E08-BODY
                                       TO WRK-TAM-JSON
                    MULTIPLY WRK-TAM-JSON BY 50 GIVING WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE19-MAPA-2
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE19-MAPA-2
                    COMPUTE FRWKW203-TAM-AREA1 =
                         LENGTH OF BVVEWE19-MAPA-2 + 2000
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E08-BODY
                    MOVE LENGTH OF BVVEWMAP-E08-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 9
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E09-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE19-MAPA-3
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE19-MAPA-3
                    MOVE LENGTH OF BVVEWE19-MAPA-3
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E09-BODY
                    MOVE LENGTH OF BVVEWMAP-E09-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 10
                    SET FRWKW203-IND-BL-OUTRO-RQS TO TRUE
                    MOVE ZEROS         TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE19-MAPA-4
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE19-MAPA-4
                    MOVE LENGTH OF BVVEWE19-MAPA-4
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO NULL
                    MOVE ZEROS                  TO FRWKW203-TAM-AREA2
               WHEN 12
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E12-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE19-MAPA-5
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE19-MAPA-5
                    MOVE LENGTH OF BVVEWE19-MAPA-5
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E12-BODY
                    MOVE LENGTH OF BVVEWMAP-E12-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 13
                    SET FRWKW203-IND-BL-OUTRO-RQS TO TRUE
                    MOVE ZEROS         TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE19-MAPA-4
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE19-MAPA-4
                    MOVE LENGTH OF BVVEWE19-MAPA-4
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO NULL
                    MOVE ZEROS                  TO FRWKW203-TAM-AREA2
               WHEN 15
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E15-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE20-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE20-MAPA
                    MOVE LENGTH OF BVVEWE20-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E15-BODY
                    MOVE LENGTH OF BVVEWMAP-E15-BODY
                                       TO FRWKW203-TAM-AREA2
               WHEN 18
                    SET FRWKW203-IND-BL-OUTRO-JS  TO TRUE
                    MOVE LENGTH OF BVVEWMAP-E18-BODY
                                       TO WRK-TAM-JSON
                    MOVE LENGTH OF BVVEWE25-MAPA
                                       TO WRK-TAM-COBOL
                    COMPUTE WRK-SOMA-TAMANHO = WRK-TAM-JSON +
                                               WRK-TAM-COBOL
                    SET  FRWKW203-PTR-AREA1     TO
                                      ADDRESS OF BVVEWE25-MAPA
                    MOVE LENGTH OF BVVEWE25-MAPA
                                       TO FRWKW203-TAM-AREA1
                    SET  FRWKW203-PTR-AREA2     TO
                                      ADDRESS OF BVVEWMAP-E18-BODY
                    MOVE LENGTH OF BVVEWMAP-E18-BODY
                                       TO FRWKW203-TAM-AREA2
           END-EVALUATE.

           MOVE SPACES                 TO FRWKW203-IDENT-AREA-GLOBAL

           MOVE 'BVVEWENT'             TO FRWKW203-ID-BLOCO
           MOVE WRK-SOMA-TAMANHO       TO FRWKW203-TAM-BLOCO
           PERFORM  8000-ACIONAR-FRWK1200.

       2050-99-FIM. EXIT.

      *----------------------------------------------------------------*
       2060-CRIAR-BLOCO-EEXT           SECTION.

           SET  EEXTW301-VERSAO-002    TO   TRUE
           SET  EEXTW301-POST          TO   TRUE
           MOVE 'BVVEXAAA'             TO   EEXTW301-SERVICO
           MOVE 'BVVEWENT'             TO   EEXTW301-CONTAINER-RQST
           MOVE 99999                  TO   EEXTW301-TAM-CONTAINER-RQST
           MOVE 'BVVEWSAI'             TO   EEXTW301-CONTAINER-RPSE
           MOVE 99999                  TO   EEXTW301-TAM-CONTAINER-RPSE

           INITIALIZE  FRWKW203-BLOCO-ENTRADA
                       FRWKW203-BLOCO-RETORNO
                       FRWKW203-BLOCO-ENTRADA-SAIDA.

           SET FRWKW203-FUN-CRIAR      TO   TRUE
           SET FRWKW203-PERM-ATUALIZAR TO   TRUE
           SET FRWKW203-IND-BL-OUTRO   TO   TRUE
           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO   FRWKW203-IDENT-AREA-GLOBAL

           MOVE EEXTW301-COD-LAYOUT    TO FRWKW203-ID-BLOCO
           MOVE EEXTW301-TAM-LAYOUT    TO FRWKW203-TAM-BLOCO
           SET  FRWKW203-PTR-AREA1     TO ADDRESS
                                       OF WRK-EEXTW301
           MOVE LENGTH OF WRK-EEXTW301 TO FRWKW203-TAM-AREA1
           SET  FRWKW203-PTR-AREA2     TO NULL
           MOVE ZEROS                  TO FRWKW203-TAM-AREA2

           PERFORM 8000-ACIONAR-FRWK1200.

       2060-99-FIM. EXIT.

      *----------------------------------------------------------------*
       2300-CONEXAO-EEXT               SECTION.

           INITIALIZE  FRWKW500-BLOCO-RETORNO
                       FRWKW500-REGISTRO.

           MOVE ZEROS                  TO FRWKGLAQ-COD-RETORNO
                                          FRWKGLAQ-PROTOCOLO.
           MOVE SPACES                 TO FRWKGLAQ-COD-ERRO
                                          FRWKGLAQ-COD-MENSAGEM
                                          FRWKGLAQ-IDENT-AREA-GLOBAL.
           MOVE 'EEXT301X'             TO FRWKW500-COD-PROGRAMA.
           MOVE LNK-AREA-IFRWKAQ       TO FRWKW500-BOOK-FRWKWAAQ.

           EXEC  CICS LINK
                      PROGRAM  (WRK-FRWK1500)
                      COMMAREA (WRK-FRWKW500)
                      LENGTH   (LENGTH OF WRK-FRWKW500)
                      NOHANDLE
           END-EXEC.

           IF EIBRESP NOT EQUAL  DFHRESP(NORMAL)
              PERFORM 9999-API-ERROS
           END-IF.

           IF FRWKW500-COD-RETORNO NOT EQUAL ZEROS
              PERFORM 9999-API-ERROS
           END-IF.

           MOVE FRWKW500-BOOK-FRWKWAAQ
                                       TO LNK-AREA-IFRWKAQ.

           IF FRWKGLAQ-COD-RETORNO NOT EQUAL ZEROS
              IF FRWKGLAQ-COD-RETORNO  EQUAL 16     AND
                 FRWKGLAQ-COD-ERRO     EQUAL '0015' AND
                 FRWKGLAQ-COD-MENSAGEM EQUAL 'EEXT0140'
                 INITIALIZE  WRK-BVVE3519-COMMAREA
                 MOVE 0                      TO BVVEWM20-STATUS-CONV
                 MOVE SPACES                 TO BVVEWM20-MSGERR-CONV
                 MOVE 1                      TO BVVEWM20-STATUS-OPEN
                 MOVE 'Time-out na comunicacao' TO BVVEWM20-MSGERR-OPEN
                 PERFORM 2003-GRAVA-MONITOR
                 MOVE 08               TO BVVEWE00-CODRET
                 MOVE 'TIMEOUT'        TO BVVEWE00-MSG
                 PERFORM 4000-FINALIZAR
              END-IF
              IF FRWKGLAQ-COD-RETORNO EQUAL 16  AND
                 (FRWKGLAQ-COD-ERRO   EQUAL '0016' OR '0017')
                  PERFORM 2310-OBTER-BLOCO-EEXTW301
                  MOVE EEXTW301-COD-RETORNO
                                       TO BVVEWE00-CODRET
                  MOVE EEXTW301-COD-MENSAGEM
                                       TO BVVEWE00-MSG
                  PERFORM 9500-ERRO-MODULO
              END-IF
              INITIALIZE  WRK-BVVE3519-COMMAREA
              MOVE 0                      TO BVVEWM20-STATUS-CONV
              MOVE SPACES                 TO BVVEWM20-MSGERR-CONV
              MOVE 1                      TO BVVEWM20-STATUS-OPEN
              MOVE 'Falha de comunicacao' TO BVVEWM20-MSGERR-OPEN
              PERFORM 2003-GRAVA-MONITOR
              MOVE FRWKGLAQ-COD-MENSAGEM
                                       TO BVVEWE00-MSG
              MOVE FRWKGLAQ-COD-RETORNO
                                       TO BVVEWE00-CODRET
              MOVE WRK-FRWK1500        TO FRWKW99B-MD-NOME-MODULO
              MOVE FRWKGLAQ-BLOCO-RETORNO
                                       TO FRWKW99B-MD-BLOCO-RETORNO
              MOVE '2300-CONEXAO-EEXT' TO FRWKW99B-HE-IDEN-PARAGRAFO
              PERFORM 9500-ERRO-MODULO
              PERFORM 4000-FINALIZAR
           ELSE
              PERFORM 2310-OBTER-BLOCO-EEXTW301
           END-IF.

       2300-99-FIM. EXIT.

      *---------------------------------------------------------------*
       2310-OBTER-BLOCO-EEXTW301       SECTION.

           INITIALIZE FRWKW203-BLOCO-ENTRADA
                      FRWKW203-BLOCO-RETORNO
                      FRWKW203-BLOCO-ENTRADA-SAIDA.

           SET FRWKW203-FUN-CONSULTAR  TO TRUE.
           SET FRWKW203-PERM-ATUALIZAR TO TRUE.
           SET FRWKW203-IND-BL-OUTRO   TO TRUE.

           MOVE FRWKGLAQ-IDENT-AREA-GLOBAL
                                       TO FRWKW203-IDENT-AREA-GLOBAL.
           MOVE EEXTW301-COD-LAYOUT    TO FRWKW203-ID-BLOCO.
           MOVE LENGTH OF WRK-EEXTW301 TO FRWKW203-TAM-BLOCO.

           SET FRWKW203-PTR-AREA1      TO ADDRESS OF WRK-EEXTW301.

           MOVE LENGTH OF WRK-EEXTW301 TO FRWKW203-TAM-AREA1.

           PERFORM 8000-ACIONAR-FRWK1200.

       2310-99-FIM. EXIT.

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.

           EXEC CICS
                RETURN
           END-EXEC.

       4000-99-FIM. EXIT.

      *----------------------------------------------------------------*
       8000-ACIONAR-FRWK1200           SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                     PROGRAM  (WRK-FRWK1200)
                     COMMAREA (WRK-FRWKW203)
                     LENGTH   (LENGTH OF WRK-FRWKW203)
                     NOHANDLE
           END-EXEC.

           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
              MOVE FRWKW203-COD-RETORNO
                                       TO FRWKGLAQ-COD-RETORNO
                                          BVVEWE00-CODRET
              MOVE FRWKW203-COD-ERRO   TO FRWKGLAQ-COD-ERRO
              MOVE FRWKW203-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
                                          BVVEWE00-MSG
              INITIALIZE FRWKW99B-CI-REGISTRO
              MOVE '8000-ACIONAR-FRWK1200'
                                       TO FRWKW99B-HE-IDEN-PARAGRAFO
              PERFORM 9999-API-ERROS
              PERFORM 4000-FINALIZAR
           END-IF.

           IF FRWKW203-COD-RETORNO NOT EQUAL ZEROS

              INITIALIZE  WRK-BVVE3519-COMMAREA
              MOVE 'Falha na conversao'   TO WRK-MONITOR
              MOVE 1                      TO BVVEWM20-STATUS-CONV
              MOVE 'Falha no programa de conversao'
                                          TO BVVEWM20-MSGERR-CONV
              MOVE 0                      TO BVVEWM20-STATUS-OPEN
              MOVE SPACES                 TO BVVEWM20-MSGERR-OPEN
              PERFORM 2003-GRAVA-MONITOR
              MOVE FRWKW203-COD-RETORNO
                                       TO FRWKGLAQ-COD-RETORNO
                                          BVVEWE00-CODRET
              MOVE FRWKW203-COD-ERRO   TO FRWKGLAQ-COD-ERRO
              MOVE FRWKW203-COD-MENSAGEM
                                       TO FRWKGLAQ-COD-MENSAGEM
                                          BVVEWE00-MSG
              INITIALIZE FRWKW99B-CI-REGISTRO
              MOVE WRK-FRWK1200        TO FRWKW99B-MD-NOME-MODULO
              MOVE FRWKW203-BLOCO-RETORNO
                                       TO FRWKW99B-MD-BLOCO-RETORNO
              MOVE '8000-ACIONAR-FRWK1200'
                                       TO FRWKW99B-HE-IDEN-PARAGRAFO
              PERFORM 9999-API-ERROS
              PERFORM 4000-FINALIZAR
           END-IF.

       8000-99-FIM. EXIT.

      *----------------------------------------------------------------*
       9500-ERRO-MODULO                SECTION.

           MOVE 16                     TO FRWKGLAQ-COD-RETORNO.
           MOVE WRK-PROGRAMA           TO FRWKGHEA-NOME-PROGRAMA.
           MOVE FRWKGMOD-TAM-LAYOUT    TO FRWKGHEA-TAM-DADOS.
           MOVE WRK-COPY-MOD           TO WRK-BLOCO-INFO-ERRO.

           PERFORM 9999-API-ERROS.

       9500-99-FIM. EXIT.

      *----------------------------------------------------------------*
      *    ROTINA PARA GRAVAR LOG DE ERRO CHAMADA AO FRWK1999.         *
      *----------------------------------------------------------------*
       9999-API-ERROS                  SECTION.
      *----------------------------------------------------------------*

           EXEC CICS LINK
                     PROGRAM  (WRK-FRWK1999)
                     COMMAREA (WRK-AREA-ERRO)
                     LENGTH   (LENGTH OF WRK-AREA-ERRO)
                     NOHANDLE
           END-EXEC.

           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
              CONTINUE
           END-IF

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *----------------------------------------------------------------*
