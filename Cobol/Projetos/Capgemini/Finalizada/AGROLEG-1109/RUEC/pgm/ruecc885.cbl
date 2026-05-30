      *===============================================================*
       IDENTIFICATION                  DIVISION.
      *===============================================================*

       PROGRAM-ID.    RUECC885.
       AUTHOR.        EDUARDO WATANABE.

      *===============================================================*
      *                  S O N D A   P R O C W O R K                  *
      *---------------------------------------------------------------*
      *                                                               *
      *      PROGRAMADOR  : EDUARDO WATANABE                          *
      *      ANALISTA     : EDUARDO WATANABE                          *
      *      DATA         : 04/02/2016                                *
      *                                                               *
      *      OBJETIVO     : ENVIAR COR0001A DAS LIBERACOES.           *
      *                     PROGRAMA CLONE DO RUEC0885.               *
      *                                                               *
      *      ARQUIVOS                                                 *
      *        E/S DDNAME   LAYOUT    DESCRICAO                       *
CAT408*        I   ENTDESCL --------  OPERACOES DESCLASSIFICADAS      *
      *        I   ARQDATAC I#RURACR  ARQUIVO DE DATAS DO SISTEMA RURA*
      *        O   ARQLIBER --------  ARQUIVO LIBERACOES              *
CAT408*        O   SAIGPDES --------  GRUPOS DE DESCLASSIFICACAO      *
      *                                                               *
      *    BANCO DE DADOS:                                            *
      *      DB2                                                      *
      *      TABLE                                 INCLUDE/BOOK       *
      *                                                               *
      *    MODULOS CHAMADOS:                                          *
      *      BRAD0650 - TESTE DE BITS                                 *
      *      BRAD7100 - MODULO DE TRATAMENTO DE ERRO                  *
      *                                                               *
EG0616*================================================================*
EG0916*                      A L T E R A C A O                         *
EG0916*================================================================*
EG0916*    ANALISTA....:   EVANDRO GUIMARAES       - BSI TECNOLOGIA    *
EG0916*    DATA........:   SETEMBRO/2016                               *
EG0916*    OBJETIVO....:   ADAPTACAO CATALOGO 4.08                     *
EG0916*================================================================*
BR0521*                      A L T E R A C A O                         *
BR0521*    ANALISTA....:   SIMONE                  - BRQ               *
BR0521*    DATA........:   JUNHO/2021                                  *
BR0521*    OBJETIVO....:   SUBSTITUIR I#RUECC5                         *
BR0521*================================================================*

      *===============================================================*
       ENVIRONMENT                     DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *---------------------------------------------------------------*
       FILE-CONTROL.

                       FILE   STATUS  IS  WRK-FS-CADRURAL.

CAT408     SELECT  ENTDESCL   ASSIGN  TO  UT-S-ENTDESCL
CAT408                 FILE   STATUS  IS  WRK-FS-ENTDESCL.
CAT408
           SELECT  ARQDATAC   ASSIGN  TO  UT-S-ARQDATAC
                       FILE   STATUS  IS  WRK-FS-ARQDATAC.

           SELECT  ARQLIBER   ASSIGN  TO  UT-S-ARQLIBER
                       FILE   STATUS  IS  WRK-FS-ARQLIBER.

CAT408     SELECT  SAIGPDES   ASSIGN  TO  UT-S-SAIGPDES
CAT408                 FILE   STATUS  IS  WRK-FS-SAIGPDES.
CAT408
      *===============================================================*
       DATA                            DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE                            SECTION.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    CADASTRO CREDITO RURAL                                     *
      *---------------------------------------------------------------*

       FD  CADRURAL
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01 FD-REG-CADRURAL              PIC X(350).

CAT408*---------------------------------------------------------------*
CAT408*    OPERACOES DESCLASSIFICADAS                                 *
CAT408*---------------------------------------------------------------*
CAT408
CAT408 FD  ENTDESCL
CAT408     LABEL RECORD STANDARD
CAT408     BLOCK CONTAINS  0 RECORDS
CAT408     RECORDING F.
CAT408
CAT408 01 FD-REG-ENTDESCL              PIC X(029).
      *---------------------------------------------------------------*
      *    ARQUIVO DE DATAS DO SISTEMA RURA                           *
      *---------------------------------------------------------------*

       FD  ARQDATAC
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01 FD-REG-ARQDATAC              PIC X(210).

      *---------------------------------------------------------------*'
      *    ARQUIVO EVENTO LIBERACOES                                  *
      *---------------------------------------------------------------*

       FD  ARQLIBER
           LABEL RECORD STANDARD
           BLOCK CONTAINS  0 RECORDS
           RECORDING F.

       01  FD-ARQLIBER                 PIC X(750).
CAT408*---------------------------------------------------------------*
CAT408*    OPERACOES DESCLASSIFICADAS                                 *
CAT408*---------------------------------------------------------------*
CAT408
CAT408 FD  SAIGPDES
CAT408     LABEL RECORD STANDARD
CAT408     BLOCK CONTAINS  0 RECORDS
CAT408     RECORDING F.
CAT408
CAT408 01  FD-SAIGPDES                 PIC X(500).
CAT408
      *---------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(32) VALUE
           '*** INICIO DA WORKING-STORAGE **'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
           '* AREA PARA TESTE DE FS        *'.
      *---------------------------------------------------------------*

       77  WRK-FS-CADRURAL             PIC  X(02) VALUE SPACES.
CAT408 77  WRK-FS-ENTDESCL             PIC  X(02) VALUE SPACES.
       77  WRK-FS-ARQDATAC             PIC  X(02) VALUE SPACES.
       77  WRK-FS-ARQLIBER             PIC  X(02) VALUE SPACES.
CAT408 77  WRK-FS-SAIGPDES             PIC  X(02) VALUE SPACES.

       77  WRK-BATCH                   PIC  X(08) VALUE 'BATCH'.
       77  WRK-FECHAMENTO              PIC  X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-ABERTURA                PIC  X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13) VALUE 'NA GRAVACAO'.

      *---------------------------------------------------------------*
       77  FILLER                      PIC  X(32) VALUE
           '* VARIAVEIS AUXILIARES GERAIS  *'.
      *---------------------------------------------------------------*

       77  WRK-JOBNOME                 PIC X(08)           VALUE SPACES.

       77  WRK-IND                     PIC 9(03)    COMP-3 VALUE ZEROS.
       77  WRK-TEM-LIBERACAO-DIA       PIC X(01)           VALUE 'N'.
CAT408 77  WRK-GRUPO-DESCL             PIC X(330)          VALUE SPACES.
CAT408 77  WRK-POS-INI                 PIC 9(005)          VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREAS AUXILIARES HEXA        *'.
      *---------------------------------------------------------------*
CAT408
CAT408 01  WRK-CH19                    PIC  X(19)          VALUE ZEROS.
CAT408 01  FILLER REDEFINES WRK-CH19.
CAT408     05 FILLER                   PIC  X(04).
CAT408     05 WRK-CH15                 PIC  X(15).
CAT408
CAT408 01  WRK-SZD13V2                 PIC +9(13)V99       VALUE ZEROS.
CAT408 01  FILLER                      REDEFINES WRK-SZD13V2.
CAT408     05 FILLER                   PIC  X(01).
CAT408     05 WRK-ZD13V2               PIC  9(13)V99.
CAT408     05 WRK-ZD13V2-R-CH15        REDEFINES
CAT408
       01  WRK-HEXAVISION.
           03  WRK-N13V2-COM-SINAL     PIC +9(13)V99        VALUE ZEROS.
           03  FILLER REDEFINES WRK-N13V2-COM-SINAL.
               05  FILLER              PIC  X(01).
               05  WRK-N13V2-SEM-SINAL PIC  9(13)V99.

BR0521*---------------------------------------------------------------*
.      01  FILLER                      PIC  X(32) VALUE
.          '* AREA AUXILIAR DESCLASSIFIC   *'.
.     *---------------------------------------------------------------*
.
.      01  WRK-COR0005-5.
.          05  WRK-GRUPO-COR0005-DESCLASS.
.              10  FILLER               PIC  X(006)     VALUE '{1200}'.
.              10  WRK-GP-COR0005-DESCLASS.
.                  15  WRK-DTA-DESCLASS PIC  X(008)     VALUE SPACES.
.                  15  WRK-VLR-DESCLASS PIC  X(019)     VALUE SPACES.
.                  15  WRK-MTV-DESCLASS PIC  X(003)     VALUE SPACES.
BR0521

       01  FILLER                      PIC  X(32) VALUE
           '* AREAS AUXILIARES DE DATA     *'.
      *---------------------------------------------------------------*

       01  WRK-COMPILACAO.
           03  WRK-DTCOMPILA.
               05  WRK-MMCOMPILA       PIC  X(002)         VALUE SPACES.
               05  FILLER              PIC  X(001)         VALUE SPACES.
               05  WRK-DDCOMPILA       PIC  X(002)         VALUE SPACES.
               05  FILLER              PIC  X(001)         VALUE SPACES.
               05  WRK-AACOMPILA       PIC  X(002)         VALUE SPACES.
           03  WRK-HRCOMPILA           PIC  X(008)         VALUE SPACES.

       01  WRK-DATA-ISO-N              PIC  9(009)         VALUE ZEROS.
       01  WRK-DATA-ISO    REDEFINES   WRK-DATA-ISO-N.
           03  FILLER                  PIC  X(001).
           03  WRK-ANO-ISO             PIC  X(004).
           03  WRK-MES-ISO             PIC  X(002).
           03  WRK-DIA-ISO             PIC  X(002).
CAT408 01  FILLER          REDEFINES   WRK-DATA-ISO-N.
CAT408     03  FILLER                  PIC  X(001).
CAT408      05 WRK-DATA-ISO-8N         PIC  9(008).

CAT408 01  WRK-DATA-DB2                PIC  X(010)         VALUE SPACES.
CAT408 01  FILLER                      REDEFINES WRK-DATA-DB2.
CAT408     03  WRK-DIA-DB2             PIC  X(002).
CAT408     03  FILLER                  PIC  X(001).
CAT408     03  WRK-MES-DB2             PIC  X(002).
CAT408     03  FILLER                  PIC  X(001).
CAT408     03  WRK-ANO-DB2             PIC  X(004).
CAT408
       01  WRK-DATA-EDIT.
           03  WRK-DIA-EDIT            PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-MES-EDIT            PIC  X(002)         VALUE SPACES.
           03  FILLER                  PIC  X(001)         VALUE '/'.
           03  WRK-ANO-EDIT            PIC  X(004)         VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DOS ACUMULADORES        *'.
      *---------------------------------------------------------------*
       01  ACU-LIDOS-CADRURAL          PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-LIDOS-ENTDESCL          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-LIDOS-ARQDATAC          PIC  9(09)  COMP-3  VALUE ZEROS.
       01  ACU-GRAVA-ARQLIBER          PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-GRAVA-SAIGPDES          PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-GRAVA-SAIGPDES-VAZIO    PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-DESCLASSIFICACOES       PIC  9(09)  COMP-3  VALUE ZEROS.
CAT408 01  ACU-DT-MAIOR                PIC  9(09)  COMP-3  VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO CADRURAL     *'.
      *---------------------------------------------------------------*

       COPY 'I#RURAJC'.

CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC  X(32) VALUE
CAT408     '* AREA DO ARQUIVO ENTDESCL     *'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     03  WRK-DESCL-CCONTR        PIC  9(09)   COMP-3 VALUE ZEROS.
CAT408     03  WRK-DESCL-NMOTVO-DCLAS-CONTR
CAT408                                 PIC  9(05)   COMP-3 VALUE ZEROS.
CAT408     03  WRK-DESCL-CMOTVO-DCLAS-BACEN
CAT408                                 PIC  X(03)          VALUE SPACES.
CAT408     03  WRK-DESCL-DDCLAS-OPER-CREDT
CAT408                                 PIC  X(10)          VALUE SPACES.
CAT408     03  WRK-DESCL-VPRINC-DCLAS-CONTR
CAT408                                 PIC  9(13)V99 COMP-3 VALUE ZEROS.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 01  FILLER                      PIC  X(32) VALUE
CAT408     '* AREA DO ARQUIVO SAIGPDES     *'.
CAT408*---------------------------------------------------------------*
CAT408
CAT408 01  WRK-SAIGPDES-REG.
CAT408     03  WRK-DES-CCONTR          PIC 9(009)          VALUE ZEROS.
CAT408     03  WRK-DES-VAZIO           PIC X(001)          VALUE SPACES.
CAT408     03  WRK-DES-GRUPO-DESCL     PIC X(330)          VALUE SPACES.
CAT408     03  FILLER                  PIC X(160)          VALUE SPACES.
CAT408
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO DE DATAC     *'.
      *---------------------------------------------------------------*

       COPY 'I#RURACR'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DO ARQUIVO LIBERACOES   *'.
      *---------------------------------------------------------------*

       01  WRK-ARQLIBER.
           05  WRK-LIB-CHAVE           PIC  9(09)           VALUE ZEROS.
           05  WRK-LIB-IND             PIC  9(03)    COMP-3 VALUE ZEROS.
           05  WRK-LIB-OCORRENCIAS  OCCURS  30 TIMES.
               10  WRK-LIB-DATA        PIC  X(08)           VALUE ZEROS.
               10  WRK-LIB-VALOR       PIC  9(13)V99        VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* TAB. INTERNA PARA LIBERACOES *'.

       01  WRK-TABELA.
           05  WRK-TAB-OCORRENCIAS  OCCURS 300 TIMES.
               10  WRK-TAB-DATA        PIC  X(08)           VALUE ZEROS.
               10  WRK-TAB-VALOR       PIC S9(13)V99 COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DE MENSAGENS DE ERRO    *'.
      *---------------------------------------------------------------*

       01  WRK-ERRO-ARQUIVO.
           03  FILLER                  PIC  X(08)  VALUE
               '** ERRO '.
           03  WRK-OPERACAO            PIC  X(13)  VALUE SPACES.
           03  FILLER                  PIC  X(12)  VALUE
               ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(08)  VALUE SPACES.
           03  FILLER                  PIC  X(18)  VALUE
               ' - FILE STATUS =  '.
           03  WRK-FILE-STATUS         PIC  X(02)  VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA DA BRAD0650             *'.
      *---------------------------------------------------------------*

       01  WRK-BRAD0650.
           05  WRK-MASCARA-064         PIC  9(04)  COMP  VALUE 064.
           05  WRK-COMA.
               10  WRK-BYTE01          PIC  X(01)        VALUE SPACES.
               10  FILLER              PIC  X(07)        VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA PARA TRATAMENTO DE ERRO *'.
      *---------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC 9(05) COMP-3 VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC 9(09) COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC 9(07) COMP-3 VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC 9(13) COMP-3 VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC X(20)        VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* CHAVES DE BALANCE-LINE       *'.
      *---------------------------------------------------------------*

       01  WRK-CHV-CADRURAL-ATU.
           03  WRK-CONTRATO-CADRURAL   PIC  9(09)       VALUE ZEROS.

       01  WRK-CHV-CADRURAL-ANT        PIC  X(09) VALUE LOW-VALUES.
CAT408 01  WRK-CHV-CADRURAL-ANT-N      REDEFINES
CAT408     WRK-CHV-CADRURAL-ANT        PIC  9(09).
CAT408 01  WRK-CHV-ENTDESCL-ATU.
CAT408     03  WRK-CONTRATO-ENTDESCL   PIC  9(09)       VALUE ZEROS.
CAT408
CAT408 01  WRK-CHV-ENTDESCL-ANT        PIC  X(09) VALUE LOW-VALUES.
CAT408
      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(32) VALUE
           '* AREA PARA DISPLAYS           *'.
      *---------------------------------------------------------------*

       COPY 'I#RURAYY'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50) VALUE
           '*** FIM DA WORKING-STORAGE ***'.
      *---------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

       0000-ROTINA-PRINCIPAL           SECTION.
      *---------------------------------------------------------------*

           PERFORM  1000-INICIO.

           PERFORM  2000-VERIFICAR-VAZIOS.

           PERFORM  3000-PROCESSO UNTIL WRK-FS-CADRURAL EQUAL '10'.

           PERFORM  7000-FINALIZA.

      *---------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       1000-INICIO                     SECTION.
      *---------------------------------------------------------------*

           OPEN  INPUT  CADRURAL
CAT408                  ENTDESCL
                 OUTPUT ARQLIBER
CAT408                  SAIGPDES.

           MOVE WRK-ABERTURA           TO  WRK-OPERACAO.
           PERFORM  8000-TESTAR-FILE-STATUS.

           CALL  'BRAD7600'            USING WRK-DATA-HORA.

      *---------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2000-VERIFICAR-VAZIOS           SECTION.
      *---------------------------------------------------------------*

      ***  VERIFICA VAZIO - ARQDATAC
           PERFORM  2200-LER-ARQDATAC.

           IF WRK-FS-ARQDATAC          EQUAL  '10'
              MOVE '>>> ARQUIVO ARQDATAC VAZIO, PROCESSAMENTO ENCERRADO'
              PERFORM 7000-FINALIZA
           END-IF.

      ***  VERIFICA VAZIO - CADRURAL
           PERFORM  2100-LER-CADRURAL.

           IF WRK-FS-CADRURAL          EQUAL  '10'
              MOVE '>>> ARQUIVO CADRURAL VAZIO, PROCESSAMENTO ENCERRADO'
                                       TO WRK-COMENTARIO
              PERFORM 7000-FINALIZA
           END-IF.

CAT408***  VERIFICA VAZIO - ENTDESCL
CAT408     PERFORM  2300-LER-ENTDESCL.
CAT408
CAT408     IF WRK-FS-ENTDESCL          EQUAL  '10'
CAT408        MOVE '>>> ARQUIVO ENTDESCL VAZIO'
CAT408                                 TO WRK-COMENTARIO
CAT408     END-IF.
CAT408
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2100-LER-CADRURAL               SECTION.
      *---------------------------------------------------------------*

      *---------------------*
       2100-01-LER-CADRURAL.
      *---------------------*

           READ CADRURAL               INTO N110RURA.

           IF  WRK-FS-CADRURAL         EQUAL  '10'
               MOVE  HIGH-VALUES       TO  WRK-CHV-CADRURAL-ATU
               GO TO 2100-99-FIM
           END-IF.

           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.
           PERFORM  8100-TESTAR-FS-CADRURAL.
           ADD   1                     TO  ACU-LIDOS-CADRURAL.

               ADD 1                   TO ACU-DT-MAIOR
           END-IF.

           IF (N110T100                NOT EQUAL 100) OR
              (N110DBCR                NOT EQUAL '2') OR
              (N110DTCT                EQUAL ZEROS )  OR
              (N110DTCT                NOT LESS WRK-DT-AAAAMMDD)
               GO TO 2100-01-LER-CADRURAL
           END-IF.

           MOVE  N110CHAV              TO  WRK-CONTRATO-CADRURAL.

      *--------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2200-LER-ARQDATAC               SECTION.
      *---------------------------------------------------------------*

           READ ARQDATAC               INTO N090RURA.
           IF  WRK-FS-ARQDATAC         EQUAL  '10'
               GO TO 2200-99-FIM
           END-IF.

           MOVE  WRK-LEITURA           TO  WRK-OPERACAO.
           PERFORM  8200-TESTAR-FS-ARQDATAC.
           ADD   1                     TO  ACU-LIDOS-ARQDATAC.

      *---------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

CAT408*---------------------------------------------------------------*
CAT408 2300-LER-ENTDESCL               SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------*
CAT408 2300-01-LER-ENTDESCL.
CAT408*---------------------*
CAT408
CAT408     MOVE WRK-CHV-ENTDESCL-ATU   TO WRK-CHV-ENTDESCL-ANT.
CAT408     READ ENTDESCL               INTO WRK-ENTDESCL-REG.
CAT408
CAT408     IF  WRK-FS-ENTDESCL         EQUAL  '10'
CAT408         MOVE  HIGH-VALUES       TO  WRK-CHV-ENTDESCL-ATU
CAT408         GO TO 2300-99-FIM
CAT408     END-IF.
CAT408
CAT408     MOVE  WRK-LEITURA           TO  WRK-OPERACAO.
CAT408     PERFORM  8400-TESTAR-FS-ENTDESCL.
CAT408     ADD   1                     TO  ACU-LIDOS-ENTDESCL.
CAT408
CAT408     MOVE  WRK-DESCL-CCONTR      TO  WRK-CONTRATO-ENTDESCL.
CAT408
CAT408*--------------------------------------------------------------*
CAT408 2300-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
      *---------------------------------------------------------------*
       3000-PROCESSO                   SECTION.
      *---------------------------------------------------------------*

CAT408         PERFORM 3400-CHECA-DESCLASSIFICACAO
               IF  WRK-TEM-LIBERACAO-DIA EQUAL 'S'
                   PERFORM 3300-GRAVAR
                   MOVE 'N'            TO WRK-TEM-LIBERACAO-DIA
               END-IF
               INITIALIZE              WRK-TABELA
                                       WRK-ARQLIBER
               MOVE N110CHAV           TO WRK-LIB-CHAVE
               MOVE WRK-CHV-CADRURAL-ATU TO WRK-CHV-CADRURAL-ANT
           END-IF.

           IF (N110C995                NOT GREATER  60) AND
              (N110C995                NOT EQUAL    58) AND
              (N110C995                NOT EQUAL    59) AND
              (N110PRAC                NOT EQUAL ZEROS)
               PERFORM 3100-LIBERACAO  VARYING WRK-IND FROM 1 BY 1
                                       UNTIL WRK-IND GREATER 300
           END-IF.

           IF (N110C995                EQUAL  65 OR 70) AND
              (N110PRAC                NOT EQUAL ZEROS)
                                       UNTIL WRK-IND GREATER 300
           END-IF.

      *    TESTA BIT1 -
      *    BIT1= 0 ==>                       (RETURN-CODE <> 0)
      *    BIT1= 1 ==>  PROCESSAMENTO DO DIA (RETURN-CODE =  0)

           MOVE  N110COMA               TO WRK-COMA
           CALL 'BRAD0650'           USING WRK-BYTE01
                                           WRK-MASCARA-064.

      ***
      ***  NAO VAI USAR BRAD0650 POR ENQUANTO, REVER APOS OS TESTES.
      ***
           IF  N110DTCT                EQUAL N090DTIN
               MOVE 'S'                TO WRK-TEM-LIBERACAO-DIA
           END-IF.

           PERFORM 2100-LER-CADRURAL.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       3100-LIBERACAO                  SECTION.
      *---------------------------------------------------------------*

           MOVE N110C043               TO WRK-DATA-ISO-N.

           IF (SPACES                  EQUAL WRK-TAB-DATA (WRK-IND))
               MOVE WRK-DATA-ISO (2:8) TO WRK-TAB-DATA  (WRK-IND)
               MOVE N110PRAC           TO WRK-TAB-VALOR (WRK-IND)
               MOVE 500                TO WRK-IND
           ELSE
               IF (WRK-DATA-ISO (2:8)  EQUAL WRK-TAB-DATA (WRK-IND))
                   ADD  N110PRAC       TO WRK-TAB-VALOR (WRK-IND)
                   MOVE 500            TO WRK-IND
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3100-99-FIM.                    EXIT.

      *---------------------------------------------------------------*
       3200-ESTORNO                    SECTION.
      *---------------------------------------------------------------*

           MOVE N110C043               TO WRK-DATA-ISO-N.

           IF (SPACES                  EQUAL WRK-TAB-DATA (WRK-IND))
               MOVE WRK-DATA-ISO (2:8) TO WRK-TAB-DATA  (WRK-IND)
               SUBTRACT N110PRAC       FROM WRK-TAB-VALOR (WRK-IND)
               MOVE 500                TO WRK-IND
           ELSE
               IF (WRK-DATA-ISO (2:8)  EQUAL WRK-TAB-DATA (WRK-IND))
                   SUBTRACT N110PRAC   FROM WRK-TAB-VALOR (WRK-IND)
                   MOVE 500            TO WRK-IND
               END-IF
           END-IF.

      *---------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3300-GRAVAR                     SECTION.
      *---------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1
                     UNTIL WRK-IND GREATER 300
                        OR WRK-TAB-DATA (WRK-IND) EQUAL SPACES
              IF WRK-TAB-VALOR (WRK-IND) GREATER ZEROS AND
                 WRK-LIB-IND           LESS 30
                 ADD  1                TO WRK-LIB-IND
                 MOVE WRK-TAB-DATA (WRK-IND)
                                       TO WRK-LIB-DATA (WRK-LIB-IND)
                 MOVE WRK-TAB-VALOR (WRK-IND) TO WRK-N13V2-COM-SINAL
                 MOVE WRK-N13V2-SEM-SINAL
                                       TO WRK-LIB-VALOR (WRK-LIB-IND)
              END-IF
           END-PERFORM.

           WRITE FD-ARQLIBER           FROM  WRK-ARQLIBER.

           MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.
           ADD 1                       TO ACU-GRAVA-ARQLIBER.

      *---------------------------------------------------------------*
       3300-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

CAT408*---------------------------------------------------------------*
CAT408 3400-CHECA-DESCLASSIFICACAO     SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     IF  WRK-CHV-CADRURAL-ANT    NOT NUMERIC
CAT408         GO TO 3400-99-FIM
CAT408     END-IF.
CAT408
CAT408***  VERIFICA SE O CONTRATO POSSUI DESCLASSIFICACOES CADASTRADAS
CAT408     PERFORM 2300-LER-ENTDESCL   UNTIL
CAT408         WRK-CHV-ENTDESCL-ATU    NOT LESS WRK-CHV-CADRURAL-ANT
CAT408     OR  WRK-CHV-ENTDESCL-ATU    EQUAL    HIGH-VALUES
CAT408
CAT408     IF  WRK-CHV-ENTDESCL-ATU    EQUAL WRK-CHV-CADRURAL-ANT
CAT408         PERFORM 3402-COM-DESCLASSIFICACOES
CAT408         PERFORM 3401-SEM-DESCLASSIFICACOES
CAT408     END-IF.
CAT408
CAT408     PERFORM 3404-GRAVAR-SAIGPDES.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 3400-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*----------------------------------------------------------------*
CAT408 3401-SEM-DESCLASSIFICACOES      SECTION.
CAT408*----------------------------------------------------------------*
CAT408***  CONTRATO NAO POSSUI DESCLASSIFICACOES CADASTRADAS.
CAT408***  CASO A COR0001 TENHA GRUPO DE DESCLASSIFICACAO PREENCHIDO
CAT408***  SERA NECESSARIO REENVIO, RETIRANDO ESTE GRUPO.
CAT408
CAT408     MOVE WRK-CHV-CADRURAL-ANT-N TO WRK-DES-CCONTR.
CAT408     MOVE 'S'                    TO WRK-DES-VAZIO.
CAT408     ADD  1                      TO ACU-GRAVA-SAIGPDES-VAZIO.
CAT408
CAT408     MOVE SPACES                 TO WRK-GRUPO-DESCL.
CAT408
CAT408     MOVE ALL '*'                TO WRK-DTA-DESCLASS
CAT408                                    WRK-VLR-DESCLASS
CAT408                                    WRK-MTV-DESCLASS.
CAT408
CAT408     MOVE WRK-COR0005-5          TO WRK-GRUPO-DESCL
CAT408              ( WRK-POS-INI:     LENGTH OF WRK-COR0005-5  ).
CAT408     MOVE WRK-GRUPO-DESCL        TO WRK-DES-GRUPO-DESCL.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 3401-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*----------------------------------------------------------------*
CAT408 3402-COM-DESCLASSIFICACOES      SECTION.
CAT408*----------------------------------------------------------------*
CAT408***  CONTRATO POSSUI DESCLASSIFICACOES CADASTRADAS.
CAT408***  REMONTA GRUPO DE DESCLASSIFICACAO COMPLETO PARA COMPARACAO
CAT408***  COM O GRAVADO NA COR0001.
CAT408
CAT408     MOVE WRK-CONTRATO-ENTDESCL  TO WRK-DES-CCONTR.
CAT408
CAT408     MOVE SPACES                 TO WRK-GRUPO-DESCL.
CAT408     MOVE 1                      TO WRK-POS-INI
CAT408                                    ACU-DESCLASSIFICACOES.
CAT408
CAT408     MOVE WRK-CHV-ENTDESCL-ATU   TO WRK-CHV-ENTDESCL-ANT
CAT408
CAT408     PERFORM UNTIL
CAT408         WRK-CHV-ENTDESCL-ATU    NOT EQUAL WRK-CHV-ENTDESCL-ANT
CAT408     OR  WRK-CHV-ENTDESCL-ATU    EQUAL    HIGH-VALUES
CAT408         PERFORM 3403-MONTAR-GRUPO-DESCL
CAT408         PERFORM 2300-LER-ENTDESCL
CAT408     END-PERFORM.
CAT408
CAT408     MOVE WRK-GRUPO-DESCL        TO WRK-DES-GRUPO-DESCL.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 3402-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*----------------------------------------------------------------*
CAT408*----------------------------------------------------------------*
CAT408
CAT408     IF  ACU-DESCLASSIFICACOES   GREATER 10
CAT408         DISPLAY '** GRUPO TRUNCADO. CONTRATO '
CAT408                 WRK-CHV-ENTDESCL-ATU
CAT408                 ' POSSUI MAIS DE 10 DESCLASSIFICACOES'
CAT408         GO TO 3403-99-FIM
CAT408     END-IF
CAT408
CAT408     MOVE WRK-DESCL-DDCLAS-OPER-CREDT
CAT408                                 TO WRK-DATA-DB2.
CAT408     MOVE WRK-DIA-DB2            TO WRK-DIA-ISO.
CAT408     MOVE WRK-MES-DB2            TO WRK-MES-ISO.
CAT408     MOVE WRK-ANO-DB2            TO WRK-ANO-ISO.
CAT408     MOVE WRK-DATA-ISO-8X        TO WRK-DTA-DESCLASS.
CAT408
CAT408     MOVE WRK-DESCL-VPRINC-DCLAS-CONTR
CAT408                                 TO WRK-SZD13V2.
CAT408     MOVE WRK-ZD13V2-R-CH15      TO WRK-CH15.
CAT408     MOVE WRK-CH19               TO WRK-VLR-DESCLASS.
CAT408
CAT408                                 TO WRK-MTV-DESCLASS.
CAT408     MOVE WRK-COR0005-5          TO WRK-GRUPO-DESCL
CAT408          ( WRK-POS-INI:         LENGTH OF WRK-COR0005-5  ).
CAT408     ADD  LENGTH OF WRK-COR0005-5
CAT408                                 TO WRK-POS-INI.
CAT408     MOVE WRK-GRUPO-DESCL        TO WRK-DES-GRUPO-DESCL.
CAT408
CAT408     ADD  1                      TO ACU-DESCLASSIFICACOES.
CAT408
CAT408*----------------------------------------------------------------*
CAT408 3403-99-FIM.                    EXIT.
CAT408*----------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 3404-GRAVAR-SAIGPDES            SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     WRITE FD-SAIGPDES           FROM WRK-SAIGPDES-REG.
CAT408
CAT408     MOVE  WRK-GRAVACAO          TO WRK-OPERACAO.
CAT408     PERFORM 8500-TESTAR-FS-SAIGPDES.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 3404-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
      *---------------------------------------------------------------*
       7000-FINALIZA                   SECTION.
      *---------------------------------------------------------------*

           IF  WRK-TEM-LIBERACAO-DIA   EQUAL  'S'
               PERFORM 3300-GRAVAR
           END-IF.

CAT408     PERFORM  3400-CHECA-DESCLASSIFICACAO.
CAT408
           PERFORM  7100-DISPLAY-TOTAIS.

           CLOSE  CADRURAL
CAT408            ENTDESCL
                  ARQDATAC
                  ARQLIBER

           MOVE  WRK-FECHAMENTO        TO  WRK-OPERACAO.
           PERFORM  8000-TESTAR-FILE-STATUS.

           STOP RUN.

      *---------------------------------------------------------------*
       7000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       7100-DISPLAY-TOTAIS             SECTION.
      *---------------------------------------------------------------*

           STRING WRK-TIMESTAMP(07:02) '/'
                  WRK-TIMESTAMP(05:02) '/'
                  WRK-TIMESTAMP(01:04)
           DELIMITED BY SIZE           INTO WRK-DATAPROC.

           STRING WRK-TIMESTAMP(09:02) ':'
                  WRK-TIMESTAMP(11:02) ':'
           DELIMITED BY SIZE           INTO WRK-HORAPROC.

           CALL  'BRAD0160'  USING  WRK-JOBNOME WRK-VALORFAC.

           MOVE 'RUECC885'             TO WRK-PROGRAMA.
           MOVE WRK-JOBNOME            TO WRK-JOBNAME.

           MOVE N090DTIN               TO  WRK-DATA-ISO-N
           MOVE WRK-DIA-ISO            TO  WRK-DIA-EDIT
           MOVE WRK-MES-ISO            TO  WRK-MES-EDIT
           MOVE WRK-ANO-ISO            TO  WRK-ANO-EDIT
           MOVE WRK-DATA-EDIT          TO  WRK-DATAMOV.

           DISPLAY WRK-DISP1.
           DISPLAY WRK-DISP2.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP4.
           DISPLAY WRK-DISP5.
           DISPLAY WRK-DISP6.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP7.

           MOVE 'CADRURAL'                    TO WRK-DDNAME.
           MOVE 'I'                           TO WRK-I-O.
           MOVE 'CADASTRO CREDITO RURAL     ' TO WRK-DESCARQ.
           MOVE ACU-LIDOS-CADRURAL            TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

CAT408     MOVE 'ENTDESCL'                    TO WRK-DDNAME.
CAT408     MOVE 'I'                           TO WRK-I-O.
CAT408     MOVE 'DESCLASSIFIC. CADASTRADAS  ' TO WRK-DESCARQ.
CAT408     MOVE ACU-LIDOS-ENTDESCL            TO WRK-QTDEARQ.
CAT408     DISPLAY WRK-DISP9.
CAT408
           MOVE 'ARQDATAC'                    TO WRK-DDNAME.
           MOVE 'I'                           TO WRK-I-O.
           MOVE 'ARQUIVO DE DATAS SIST. RURA' TO WRK-DESCARQ.
           MOVE ACU-LIDOS-ARQDATAC            TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

           MOVE 'ARQLIBER'                    TO WRK-DDNAME.
           MOVE 'O'                           TO WRK-I-O.
           MOVE ACU-GRAVA-ARQLIBER            TO WRK-QTDEARQ.
           DISPLAY WRK-DISP9.

CAT408     MOVE 'SAIGPDES'                    TO WRK-DDNAME.
CAT408     MOVE 'O'                           TO WRK-I-O.
CAT408     MOVE 'GPS. DESCLASSIFICACAO    '   TO WRK-DESCARQ.
CAT408     MOVE ACU-GRAVA-SAIGPDES            TO WRK-QTDEARQ.
CAT408     DISPLAY WRK-DISP9.
CAT408
CAT408     MOVE 'SAIGPDES'                    TO WRK-DDNAME.
CAT408     MOVE 'O'                           TO WRK-I-O.
CAT408     MOVE 'GPS. DESCLASSIFI - VAZIO '   TO WRK-DESCARQ.
CAT408     MOVE ACU-GRAVA-SAIGPDES-VAZIO      TO WRK-QTDEARQ.
CAT408     DISPLAY WRK-DISP9.
CAT408
CAT408     MOVE 'DATA    '                    TO WRK-DDNAME.
CAT408     MOVE ' '                           TO WRK-I-O.
CAT408     MOVE 'DATA DE CONTABZCAO MAIOR '   TO WRK-DESCARQ.
CAT408     MOVE ACU-DT-MAIOR                  TO WRK-QTDEARQ.
CAT408     DISPLAY WRK-DISP9.
CAT408
           DISPLAY WRK-DISP10.
           DISPLAY WRK-DISP8.

           DISPLAY WRK-DISP3.

           MOVE WHEN-COMPILED                 TO WRK-COMPILACAO.
           MOVE WRK-DTCOMPILA                 TO WRK-DATA-COMP.
           MOVE WRK-DTCOMPILA(4:2)            TO WRK-DATA-COMP(1:2).
           MOVE WRK-DTCOMPILA(1:2)            TO WRK-DATA-COMP(4:2).
           MOVE WRK-HRCOMPILA                 TO WRK-HORA-COMP.
           DISPLAY WRK-DISP16.

           IF  WRK-COMENTARIO          NOT EQUAL SPACES AND LOW-VALUES
               DISPLAY WRK-DISP3
               DISPLAY WRK-DISP13
           END-IF.

           DISPLAY  WRK-DISP1.

      *---------------------------------------------------------------*
       7100-99-FIM.                    EXIT.

      *---------------------------------------------------------------*
       8000-TESTAR-FILE-STATUS         SECTION.
      *---------------------------------------------------------------*

           PERFORM 8100-TESTAR-FS-CADRURAL.

           PERFORM 8200-TESTAR-FS-ARQDATAC.

           PERFORM 8300-TESTAR-FS-ARQLIBER.

CAT408     PERFORM 8400-TESTAR-FS-ENTDESCL.
CAT408
CAT408     PERFORM 8500-TESTAR-FS-SAIGPDES.
CAT408
      *---------------------------------------------------------------*
       8000-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8100-TESTAR-FS-CADRURAL         SECTION.

           IF  WRK-FS-CADRURAL        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'CADRURAL'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-CADRURAL    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8100-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8200-TESTAR-FS-ARQDATAC         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQDATAC        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'ARQDATAC'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-ARQDATAC    TO  WRK-FILE-STATUS
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8200-99-FIM.                    EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       8300-TESTAR-FS-ARQLIBER         SECTION.
      *---------------------------------------------------------------*

           IF  WRK-FS-ARQLIBER        NOT  EQUAL '00'
               MOVE 'APL'              TO  ERR-TIPO-ACESSO
               MOVE 'ARQLIBER'         TO  WRK-NOME-ARQUIVO
               MOVE WRK-FS-ARQLIBER    TO  WRK-FILE-STATUS
               MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       8300-99-FIM.                    EXIT.

CAT408*---------------------------------------------------------------*
CAT408 8400-TESTAR-FS-ENTDESCL         SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408     IF  WRK-FS-ENTDESCL        NOT  EQUAL '00'
CAT408         MOVE 'APL'              TO  ERR-TIPO-ACESSO
CAT408         MOVE 'ENTDESCL'         TO  WRK-NOME-ARQUIVO
CAT408         MOVE WRK-FS-ENTDESCL    TO  WRK-FILE-STATUS
CAT408         MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
CAT408         PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 8400-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
CAT408*---------------------------------------------------------------*
CAT408 8500-TESTAR-FS-SAIGPDES         SECTION.
CAT408*---------------------------------------------------------------*
CAT408
CAT408         MOVE 'APL'              TO  ERR-TIPO-ACESSO
CAT408         MOVE 'SAIGPDES'         TO  WRK-NOME-ARQUIVO
CAT408         MOVE WRK-FS-SAIGPDES    TO  WRK-FILE-STATUS
CAT408         MOVE WRK-ERRO-ARQUIVO   TO  ERR-TEXTO
CAT408         PERFORM 9999-ROTINA-ERRO
CAT408     END-IF.
CAT408
CAT408*---------------------------------------------------------------*
CAT408 8500-99-FIM.                    EXIT.
CAT408*---------------------------------------------------------------*
CAT408
      *---------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *---------------------------------------------------------------*

           MOVE 'RUECC885'             TO  ERR-PGM.

           CALL 'BRAD7100'          USING  WRK-BATCH
                                           ERRO-AREA.

           GOBACK.
      *---------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *---------------------------------------------------------------*
