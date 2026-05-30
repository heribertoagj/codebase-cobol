      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*

       PROGRAM-ID. GFCT4464.

      *================================================================*
      *                  SONDA PROCWORK INFORMATICA                    *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA.........: GFCT4464                                 *
      *    PROGRAMADOR......: PAGNOCCA - SONDA IT                      *
      *                                                                *
      *    GRUPO NEGOCIO....: DDS-GP45                                 *
      *    ANALISTA.........: HELENA                                   *
      *                                                                *
      *    DATA.............: ABR/2014                                 *
      *                                                                *
      *    PROJETO..........: CESTA PJ                                 *
      *                                                                *
      *    OBJETIVO.........: GERA MOVIMENTO PARA VINCULO SELECIONADO  *
      *                       DE ADESOES INDIVIDUAIS POR               *
      *                       CESTA/COMPONENTE. PESQUISA CNPJ CLIE.    *
      *                                                                *
      *    ARQUIVOS:                                                   *
      *      DDNAME                            INCLUDE/BOOK            *
      *        ARQDATAS                          I#GFCT06              *
      *        ENTRADA                                                 *
      *        SAIDA                             GFCTWBTA              *
      *                                                                *
      *    BANCO DE DADOS...:                                          *
      *      DB2                                                       *
      *      TABLE                             INCLUDE/BOOK            *
      *        DB2PRD.PARM_SERVC_TARIF           GFCTB0A2              *
      *        DB2PRD.COMPS_TARIF_PCOTE          GFCTB029              *
      *        DB2PRD.ADSAO_INDVD_PCOTE          GFCTB009              *
      *        DB2PRD.VLR_COMPO_TARIF            GFCTB0G3              *
      *        DB2PRD.V01CLIENTE_AGENCIA         CLIEV004              *
      *        DB2PRD.V01CTA_CORRENTE            CLIEV008              *
      *                                                                *
      *    MODULOS CHAMADOS.:                                          *
      *      BRAD7100 - TRATAR ERRO                                    *
      *                                                                *
      *================================================================*

      *===============================================================*
       ENVIRONMENT DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       CONFIGURATION SECTION.
      *---------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *---------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQDATAS ASSIGN TO UT-S-ARQDATAS
                      FILE STATUS IS WRK-FS-ARQDATAS.

           SELECT ENTRADA  ASSIGN TO UT-S-ENTRADA
                      FILE STATUS IS WRK-FS-ENTRADA.

           SELECT SAIDA    ASSIGN TO UT-S-SAIDA
                      FILE STATUS IS WRK-FS-SAIDA.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 100               *
      *---------------------------------------------------------------*

       FD  ARQDATAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ARQDATAS                     PIC X(100).

      *---------------------------------------------------------------*
      *   INPUT :     ORG. SEQUENCIAL   -   LRECL = 6                 *
      *---------------------------------------------------------------*

       FD  ENTRADA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-ENT-REG.
           10 FD-CTARIF-PCOTE         PIC S9(5)V USAGE COMP-3.
           10 FD-CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.

      *---------------------------------------------------------------*
      *   OUTPUT:     ORG. SEQUENCIAL   -   LRECL = 100               *
      *---------------------------------------------------------------*

       FD  SAIDA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'GFCTWBTA'.

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       77  FILLER  PIC X(32) VALUE '*  INICIO DA WORKING GFCT4464  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

       01  WRK-CAMPO-VALOR-SINAL       PIC +9(009)V99      VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-VALOR-SINAL.
           05  WRK-CAMPO-V-SINAL-S     PIC X.
           05  WRK-CAMPO-V-SINAL-X     PIC 9(09)V99.


       01  WRK-CAMPO-TIRA-SINAL        PIC +9(017)         VALUE ZEROS.
       01  FILLER  REDEFINES           WRK-CAMPO-TIRA-SINAL.
           05  WRK-CAMPO-TIRA-SINAL-S  PIC X.
           05  WRK-CAMPO-TIRA-SINAL-17 PIC 9(17).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(01).
               10 WRK-CAMPO-TIRA-SINAL-16 PIC 9(16).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(02).
               10 WRK-CAMPO-TIRA-SINAL-15 PIC 9(15).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(03).
               10 WRK-CAMPO-TIRA-SINAL-14 PIC 9(14).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(04).
               10 WRK-CAMPO-TIRA-SINAL-13 PIC 9(13).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(05).
               10 WRK-CAMPO-TIRA-SINAL-12 PIC 9(12).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(06).
               10 WRK-CAMPO-TIRA-SINAL-11 PIC 9(11).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(07).
               10 WRK-CAMPO-TIRA-SINAL-10 PIC 9(10).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(08).
               10 WRK-CAMPO-TIRA-SINAL-09 PIC 9(09).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(09).
               10 WRK-CAMPO-TIRA-SINAL-08 PIC 9(08).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(10).
               10 WRK-CAMPO-TIRA-SINAL-07 PIC 9(07).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(11).
               10 WRK-CAMPO-TIRA-SINAL-06 PIC 9(06).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(12).
               10 WRK-CAMPO-TIRA-SINAL-05 PIC 9(05).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(13).
               10 WRK-CAMPO-TIRA-SINAL-04 PIC 9(04).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(14).
               10 WRK-CAMPO-TIRA-SINAL-03 PIC 9(03).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(15).
               10 WRK-CAMPO-TIRA-SINAL-02 PIC 9(02).
           05  FILLER REDEFINES WRK-CAMPO-TIRA-SINAL-17.
               10 FILLER                  PIC X(16).
               10 WRK-CAMPO-TIRA-SINAL-01 PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-01        PIC +9(01)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-01.
           05  WRK-CAMPO-POE-SINAL-01-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-01-C  PIC 9(01).

       01  WRK-CAMPO-POE-SINAL-02        PIC +9(02)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-02.
           05  WRK-CAMPO-POE-SINAL-02-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-02-C  PIC 9(02).

       01  WRK-CAMPO-POE-SINAL-03        PIC +9(03)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-03.
           05  WRK-CAMPO-POE-SINAL-03-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-03-C  PIC 9(03).

       01  WRK-CAMPO-POE-SINAL-04        PIC +9(04)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-04.
           05  WRK-CAMPO-POE-SINAL-04-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-04-C  PIC 9(04).

       01  WRK-CAMPO-POE-SINAL-05        PIC +9(05)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-05.
           05  WRK-CAMPO-POE-SINAL-05-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-05-C  PIC 9(05).

       01  WRK-CAMPO-POE-SINAL-06        PIC +9(06)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-06.
           05  WRK-CAMPO-POE-SINAL-06-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-06-C  PIC 9(06).

       01  WRK-CAMPO-POE-SINAL-07        PIC +9(07)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-07.
           05  WRK-CAMPO-POE-SINAL-07-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-07-C  PIC 9(07).

       01  WRK-CAMPO-POE-SINAL-08        PIC +9(08)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-08.
           05  WRK-CAMPO-POE-SINAL-08-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-08-C  PIC 9(08).

       01  WRK-CAMPO-POE-SINAL-09        PIC +9(09)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-09.
           05  WRK-CAMPO-POE-SINAL-09-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-09-C  PIC 9(09).

       01  WRK-CAMPO-POE-SINAL-10        PIC +9(10)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-10.
           05  WRK-CAMPO-POE-SINAL-10-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-10-C  PIC 9(10).

       01  WRK-CAMPO-POE-SINAL-11        PIC +9(11)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-11.
           05  WRK-CAMPO-POE-SINAL-11-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-11-C  PIC 9(11).

       01  WRK-CAMPO-POE-SINAL-12        PIC +9(12)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-12.
           05  WRK-CAMPO-POE-SINAL-12-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-12-C  PIC 9(12).

       01  WRK-CAMPO-POE-SINAL-13        PIC +9(13)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-13.
           05  WRK-CAMPO-POE-SINAL-13-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-13-C  PIC 9(13).

       01  WRK-CAMPO-POE-SINAL-14        PIC +9(14)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-14.
           05  WRK-CAMPO-POE-SINAL-14-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-14-C  PIC 9(14).

       01  WRK-CAMPO-POE-SINAL-15        PIC +9(15)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-15.
           05  WRK-CAMPO-POE-SINAL-15-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-15-C  PIC 9(15).

       01  WRK-CAMPO-POE-SINAL-16        PIC +9(16)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-16.
           05  WRK-CAMPO-POE-SINAL-16-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-16-C  PIC 9(16).

       01  WRK-CAMPO-POE-SINAL-17        PIC +9(17)      VALUE ZEROS.
       01  FILLER  REDEFINES             WRK-CAMPO-POE-SINAL-17.
           05  WRK-CAMPO-POE-SINAL-17-S  PIC X.
           05  WRK-CAMPO-POE-SINAL-17-C  PIC 9(17).

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS AUXILIARES            *'.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  AREAS DE FILE STATUS        *'.
      *---------------------------------------------------------------*

       77  WRK-BATCH                   PIC X(08) VALUE 'BATCH   '.

       77  WRK-FS-ARQDATAS             PIC X(02) VALUE SPACES.
       77  WRK-FS-ENTRADA              PIC X(02) VALUE SPACES.
       77  WRK-FS-SAIDA                PIC X(02) VALUE SPACES.
       77  WRK-NAO-ENCONTROU           PIC X(01) VALUE SPACES.
       77  WRK-FIM-CURSOR-009          PIC X(01) VALUE SPACES.
       77  WRK-OPERACAO                PIC X(13) VALUE SPACES.
       77  WRK-ABERTURA                PIC X(13) VALUE 'NA ABERTURA'.
       77  WRK-LEITURA                 PIC X(13) VALUE 'NA LEITURA'.
       77  WRK-GRAVACAO                PIC X(13) VALUE 'NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC X(13) VALUE 'NO FECHAMENTO'.
       77  WRK-COMANDO                 PIC X(010) VALUE SPACES.
       77  WRK-CESTA-PERSON-OK         PIC X(01)  VALUE SPACE.
       77  WRK-CESTA-PERSON-STA        PIC X(01)  VALUE SPACE.


      *---------------------------------------------------------------*
       77  FILLER  PIC X(32) VALUE '*  ACUMULADORES                *'.
      *---------------------------------------------------------------*

       77  ACU-LIDOS-ARQDATAS          PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-ENTRADA           PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAIDA             PIC 9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SAIDA-ERR         PIC 9(09) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  MASCARAS DE EDICAO DE TOTAIS*'.
      *---------------------------------------------------------------*

       01  FILLER.
           05  WRK-MASK-LIDOS-ARQDATAS PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-LIDOS-ENTRADA  PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-GRAVA-SAIDA    PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.
           05  WRK-MASK-GRAVA-SAIDA-ERR
                                       PIC ZZZ.ZZZ.ZZ9 VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  AREA ARQ. ARQDATAS          *'.
      *---------------------------------------------------------------*

       COPY 'I#GFCT06'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32) VALUE '*  AREAS TRATAMENTO ERRO       *'.
      *---------------------------------------------------------------*

       01  WRK-TEXTO.
           05  FILLER                  PIC X(05) VALUE 'ERRO'.
           05  WRK-OPERACAO-TXT        PIC X(13) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE ' DO ARQUIVO '.
           05  WRK-ARQ-TXT             PIC X(08) VALUE SPACES.

       COPY 'POL7100C'.

      *---------------------------------------------------------------*
       01  FILLER  PIC X(32)  VALUE  '*  AREAS DB2                   *'.
      *---------------------------------------------------------------*

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0A2
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB029
           END-EXEC.


           EXEC SQL
                INCLUDE GFCTB009
           END-EXEC.


           EXEC SQL
                INCLUDE GFCTB0G3
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV004
           END-EXEC.

           EXEC SQL
                INCLUDE CLIEV008
           END-EXEC.

           EXEC SQL
                INCLUDE GFCTB0O2
           END-EXEC.



      *---------------------------------------------------------------*
      *                  DECLARE DO CURSOR CSR01-GFCTB009
      *---------------------------------------------------------------*


           EXEC SQL
               DECLARE CSR01-GFCTB009 CURSOR FOR
               SELECT
                   CJUNC_DEPDC,
                   CCTA_CLI,
                   HINCL_REG,
                   DINIC_ADSAO_INDVD,
                   DFIM_ADSAO_INDVD,
                   CTPO_SERVC_TARIF
               FROM
                   DB2PRD.ADSAO_INDVD_PCOTE
               WHERE
                   CSERVC_TARIF        = :GFCTB009.CSERVC-TARIF
               AND CINDCD_EXCL_REG     = 0
               AND DINIC_ADSAO_INDVD  <= :GFCTB009.DINIC-ADSAO-INDVD
               AND DFIM_ADSAO_INDVD   >= :GFCTB009.DFIM-ADSAO-INDVD
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
       01  FILLER  PIC X(32) VALUE '*  FIM  DA WORKING   GFCT4464  *'.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*
           MOVE '+' TO
               WRK-CAMPO-TIRA-SINAL-S
               WRK-CAMPO-POE-SINAL-01-S
               WRK-CAMPO-POE-SINAL-02-S
               WRK-CAMPO-POE-SINAL-03-S
               WRK-CAMPO-POE-SINAL-04-S
               WRK-CAMPO-POE-SINAL-05-S
               WRK-CAMPO-POE-SINAL-06-S
               WRK-CAMPO-POE-SINAL-07-S
               WRK-CAMPO-POE-SINAL-08-S
               WRK-CAMPO-POE-SINAL-09-S
               WRK-CAMPO-POE-SINAL-10-S
               WRK-CAMPO-POE-SINAL-11-S
               WRK-CAMPO-POE-SINAL-12-S
               WRK-CAMPO-POE-SINAL-13-S
               WRK-CAMPO-POE-SINAL-14-S
               WRK-CAMPO-POE-SINAL-15-S
               WRK-CAMPO-POE-SINAL-16-S
               WRK-CAMPO-POE-SINAL-17-S.

           MOVE ZEROS TO
               WRK-CAMPO-TIRA-SINAL
               WRK-CAMPO-TIRA-SINAL-17
               WRK-CAMPO-TIRA-SINAL-16
               WRK-CAMPO-TIRA-SINAL-15
               WRK-CAMPO-TIRA-SINAL-14
               WRK-CAMPO-TIRA-SINAL-13
               WRK-CAMPO-TIRA-SINAL-12
               WRK-CAMPO-TIRA-SINAL-11
               WRK-CAMPO-TIRA-SINAL-10
               WRK-CAMPO-TIRA-SINAL-09
               WRK-CAMPO-TIRA-SINAL-08
               WRK-CAMPO-TIRA-SINAL-07
               WRK-CAMPO-TIRA-SINAL-06
               WRK-CAMPO-TIRA-SINAL-05
               WRK-CAMPO-TIRA-SINAL-04
               WRK-CAMPO-TIRA-SINAL-03
               WRK-CAMPO-TIRA-SINAL-02
               WRK-CAMPO-TIRA-SINAL-01
               WRK-CAMPO-POE-SINAL-01
               WRK-CAMPO-POE-SINAL-01-C
               WRK-CAMPO-POE-SINAL-02
               WRK-CAMPO-POE-SINAL-02-C
               WRK-CAMPO-POE-SINAL-03
               WRK-CAMPO-POE-SINAL-03-C
               WRK-CAMPO-POE-SINAL-04
               WRK-CAMPO-POE-SINAL-04-C
               WRK-CAMPO-POE-SINAL-05
               WRK-CAMPO-POE-SINAL-05-C
               WRK-CAMPO-POE-SINAL-06
               WRK-CAMPO-POE-SINAL-06-C
               WRK-CAMPO-POE-SINAL-07
               WRK-CAMPO-POE-SINAL-07-C
               WRK-CAMPO-POE-SINAL-08
               WRK-CAMPO-POE-SINAL-08-C
               WRK-CAMPO-POE-SINAL-09
               WRK-CAMPO-POE-SINAL-09-C
               WRK-CAMPO-POE-SINAL-10
               WRK-CAMPO-POE-SINAL-10-C
               WRK-CAMPO-POE-SINAL-11
               WRK-CAMPO-POE-SINAL-11-C
               WRK-CAMPO-POE-SINAL-12
               WRK-CAMPO-POE-SINAL-12-C
               WRK-CAMPO-POE-SINAL-13
               WRK-CAMPO-POE-SINAL-13-C
               WRK-CAMPO-POE-SINAL-14
               WRK-CAMPO-POE-SINAL-14-C
               WRK-CAMPO-POE-SINAL-15
               WRK-CAMPO-POE-SINAL-15-C
               WRK-CAMPO-POE-SINAL-16
               WRK-CAMPO-POE-SINAL-16-C
               WRK-CAMPO-POE-SINAL-17
               WRK-CAMPO-POE-SINAL-17-C.


      *===============================================================*
      *    ROTINA INICIAR
      *===============================================================*
       0000-INICIAR SECTION.
      *---------------------------------------------------------------*

           CALL    'POOL1050'.

           OPEN    INPUT   ARQDATAS
                           ENTRADA
                   OUTPUT  SAIDA.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           PERFORM 2000-PRIMEIRA-LEITURA.

           IF  ACU-LIDOS-ENTRADA      EQUAL   ZEROS
               GO  TO 0000-FINALIZAR
           END-IF.

           PERFORM 3000-PROCESSA
             UNTIL WRK-FS-ENTRADA     EQUAL '10'.

           PERFORM 4000-ROTINA-FINAL.

      *===============*
      * LABEL FINALIZAR
      *===============*
       0000-FINALIZAR.
      *---------------*

           CLOSE   ARQDATAS
                   ENTRADA
                   SAIDA.

           MOVE    WRK-FECHAMENTO      TO   WRK-OPERACAO.
           PERFORM 1000-TESTAR-FILE-STATUS.

           STOP    RUN.

      *---------------------------------------------------------------*
       0000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FILE-STATUS
      *===============================================================*
       1000-TESTAR-FILE-STATUS SECTION.
      *---------------------------------------------------------------*

           PERFORM 1100-TESTAR-FS-ARQDATAS.

           PERFORM 1200-TESTAR-FS-ENTRADA.

           PERFORM 1300-TESTAR-FS-SAIDA.

      *---------------------------------------------------------------*
       1000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ARQDATAS
      *===============================================================*
       1100-TESTAR-FS-ARQDATAS SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ARQDATAS NOT EQUAL '00'
              DISPLAY '************** GFCT4464 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ARQDATAS             *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ARQDATAS
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4464 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'ARQDATAS'          TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-ENTRADA
      *===============================================================*
       1200-TESTAR-FS-ENTRADA SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-ENTRADA NOT EQUAL '00'
              DISPLAY '************** GFCT4464 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*              ENTRADA              *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-ENTRADA
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4464 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'ENTRADA'           TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA TESTAR-FS-SAIDA
      *===============================================================*
       1300-TESTAR-FS-SAIDA   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-SAIDA   NOT EQUAL '00'
              DISPLAY '************** GFCT4464 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*               SAIDA               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-SAIDA
                                                 '         *'
              DISPLAY '*        PROGRAMA  ENCERRADO        *'
              DISPLAY '************** GFCT4464 *************'
              MOVE WRK-OPERACAO        TO  WRK-OPERACAO-TXT
              MOVE 'SAIDA'             TO  WRK-ARQ-TXT
              MOVE WRK-TEXTO           TO  ERR-TEXTO
              PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PRIMEIRA-LEITURA
      *===============================================================*
       2000-PRIMEIRA-LEITURA SECTION.
      *---------------------------------------------------------------*

           PERFORM 2100-LER-ARQDATAS.

           IF  ACU-LIDOS-ARQDATAS      EQUAL   ZEROS
               DISPLAY '************** GFCT4464 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO ARQDATAS ESTA VAZIO    *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4464 *************'
               GO                      TO  2000-01-ROTINA-INICIAL
           END-IF.

           PERFORM UNTIL GFCT06-CSIST-PRINC EQUAL 'GFCT' OR
                         WRK-FS-ARQDATAS    EQUAL '10'
               PERFORM 2100-LER-ARQDATAS
           END-PERFORM.

           IF  WRK-FS-ARQDATAS         EQUAL '10'
               DISPLAY '************** GFCT4464 *************'
               DISPLAY '*                                   *'
               DISPLAY '*   DATA PROC GFCT NAO LOCALIZADA   *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4464 *************'
               MOVE 'S'                TO  WRK-NAO-ENCONTROU
           END-IF.

      *=======================*
      * LABEL ROTINA-INICIAL
      *=======================*
       2000-01-ROTINA-INICIAL.
      *-----------------------*

           IF  ACU-LIDOS-ARQDATAS      EQUAL ZEROS OR
               WRK-NAO-ENCONTROU       EQUAL 'S'
               DISPLAY '************** GFCT4464 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    DATA DO GFCT NAO LOCALIZADA    *'
               DISPLAY '*                                   *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4464 *************'
           END-IF.

           PERFORM 2200-LER-ENTRADA.

           IF  ACU-LIDOS-ENTRADA      EQUAL   ZEROS
               DISPLAY '************** GFCT4464 *************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO ENTRADA ESTA VAZIO     *'
               DISPLAY '*                                   *'
               DISPLAY '*         PROGRAMA ENCERRADO        *'
               DISPLAY '*                                   *'
               DISPLAY '************** GFCT4464 *************'
           END-IF.

      *---------------------------------------------------------------*
       2000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ARQDATAS
      *===============================================================*
       2100-LER-ARQDATAS SECTION.
      *---------------------------------------------------------------*

           READ ARQDATAS INTO GFCT06-REG.

           IF  WRK-FS-ARQDATAS         EQUAL   '10'
               GO                      TO  2100-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FS-ARQDATAS.

           ADD 1                       TO  ACU-LIDOS-ARQDATAS.

      *---------------------------------------------------------------*
       2100-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA LER-ENTRADA
      *===============================================================*
       2200-LER-ENTRADA SECTION.
      *---------------------------------------------------------------*

           READ ENTRADA.

           IF  WRK-FS-ENTRADA          EQUAL   '10'
               GO                      TO  2200-99-FIM
           END-IF.

           MOVE    WRK-LEITURA         TO  WRK-OPERACAO.
           PERFORM 1200-TESTAR-FS-ENTRADA.

           ADD 1                       TO  ACU-LIDOS-ENTRADA.

           INITIALIZE GFCTWBTA-REG.

      *---------------------------------------------------------------*
       2200-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2500-TRATA-CURSOR-009 SECTION.
      *---------------------------------------------------------------*

           PERFORM 2510-OPEN-CURSOR-009.

           MOVE 'N'                    TO WRK-FIM-CURSOR-009.

           PERFORM 2520-LER-CURSOR-009 UNTIL
               WRK-FIM-CURSOR-009 EQUAL 'S'.

           PERFORM 2530-FECHAR-CURSOR-009.

      *---------------------------------------------------------------*
       2500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2510-OPEN-CURSOR-009 SECTION.
      *---------------------------------------------------------------*

           MOVE FD-CTARIF-PCOTE             TO
                CSERVC-TARIF OF GFCTB009.
           MOVE GFCT06-DPROCM-ATUAL         TO
                DINIC-ADSAO-INDVD OF GFCTB009
                DFIM-ADSAO-INDVD  OF GFCTB009.

           EXEC SQL
                OPEN CSR01-GFCTB009
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'OPEN'               TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE'   TO  ERR-DBD-TAB
                MOVE 'OPEN  '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0001'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2510-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2520-LER-CURSOR-009 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                FETCH CSR01-GFCTB009
                INTO
                :GFCTB009.CJUNC-DEPDC,
                :GFCTB009.CCTA-CLI,
                :GFCTB009.HINCL-REG,
                :GFCTB009.DINIC-ADSAO-INDVD,
                :GFCTB009.DFIM-ADSAO-INDVD,
                :GFCTB009.CTPO-SERVC-TARIF
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'FETCH'               TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE'   TO  ERR-DBD-TAB
                MOVE 'FETCH  '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0021'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL +100
              MOVE 'S' TO WRK-FIM-CURSOR-009
              GO TO 2520-99-FIM
           END-IF.

           MOVE CJUNC-DEPDC OF GFCTB009 TO
                WRK-CAMPO-TIRA-SINAL.
           MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
           MOVE WRK-CAMPO-TIRA-SINAL-05 TO WBTA-CJUNC-DEPDC.
           MOVE CCTA-CLI    OF GFCTB009 TO
                WRK-CAMPO-TIRA-SINAL.
           MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
           MOVE WRK-CAMPO-TIRA-SINAL-07 TO WBTA-CCTA-CLI.
           MOVE ZEROS                   TO
                WBTA-QFRANQ-COMPO-PCOTE
                WBTA-VCOMPO-TARIF
                WBTA-VEXCED-COMPO-PCOTE
                WBTA-VCESTA-TARIF
                WBTA-CNPJ-PRINC
                WBTA-CNPJ-FILIAL
                WBTA-CNPJ-CONTR.
           MOVE FD-CTARIF-PCOTE         TO
                WRK-CAMPO-TIRA-SINAL.
           MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
           MOVE WRK-CAMPO-TIRA-SINAL-05 TO WBTA-CCESTA-TARIF.
           MOVE FD-CSERVC-TARIF         TO
                WRK-CAMPO-TIRA-SINAL.
           MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
           MOVE WRK-CAMPO-TIRA-SINAL-05 TO WBTA-CTARIFA-TARIF.
           MOVE DINIC-ADSAO-INDVD    OF GFCTB009 TO
                WBTA-DINIC-ADSAO-INDVD.
           MOVE DFIM-ADSAO-INDVD    OF GFCTB009 TO
                WBTA-DFIM-ADSAO-INDVD.
           MOVE '  '                TO WBTA-CD-RETORNO.

           MOVE FD-CTARIF-PCOTE         TO
                CSERVC-TARIF OF GFCTB0A2.
           MOVE GFCT06-DPROCM-ATUAL TO
                DINIC-VGCIA-TARIF OF GFCTB0A2.

           EXEC SQL
                SELECT DINIC_VGCIA_TARIF,
                       CSEQ_MANUT_SERVC
                INTO
                       :GFCTB0A2.DINIC-VGCIA-TARIF,
                       :GFCTB0A2.CSEQ-MANUT-SERVC
                FROM DB2PRD.PARM_SERVC_TARIF
                WHERE
                     CSERVC_TARIF = :GFCTB0A2.CSERVC-TARIF
                     AND
                     DINIC_VGCIA_TARIF  <= :GFCTB0A2.DINIC-VGCIA-TARIF
                     AND
                     DFIM_VGCIA_TARIF   >= :GFCTB0A2.DINIC-VGCIA-TARIF
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0121'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE 'A2' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.

           MOVE FD-CTARIF-PCOTE         TO
                CSERVC-TARIF OF GFCTB0G3.
           MOVE DINIC-VGCIA-TARIF OF GFCTB0A2 TO
                DINIC-VGCIA-TARIF OF GFCTB0G3.

           EXEC SQL
                SELECT VCOMPO_TARIF
                INTO
                  :GFCTB0G3.VCOMPO-TARIF
                FROM DB2PRD.VLR_COMPO_TARIF
                WHERE
                    CSERVC_TARIF = :GFCTB0G3.CSERVC-TARIF
                AND DINIC_VGCIA_TARIF = :GFCTB0G3.DINIC-VGCIA-TARIF
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'VLR_COMPO_TARIF'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0141'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE 'G3' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.

           MOVE VCOMPO-TARIF OF GFCTB0G3 TO WRK-CAMPO-VALOR-SINAL.
           MOVE '+'                      TO WRK-CAMPO-V-SINAL-S.
           MOVE WRK-CAMPO-V-SINAL-X      TO WBTA-VCESTA-TARIF.

           MOVE FD-CSERVC-TARIF          TO
                CSERVC-TARIF OF GFCTB0A2.
           MOVE GFCT06-DPROCM-ATUAL TO
                DINIC-VGCIA-TARIF OF GFCTB0A2.

           EXEC SQL
                SELECT DINIC_VGCIA_TARIF,
                       CSEQ_MANUT_SERVC
                INTO
                       :GFCTB0A2.DINIC-VGCIA-TARIF,
                       :GFCTB0A2.CSEQ-MANUT-SERVC
                FROM DB2PRD.PARM_SERVC_TARIF
                WHERE
                     CSERVC_TARIF = :GFCTB0A2.CSERVC-TARIF
                     AND
                     DINIC_VGCIA_TARIF  <= :GFCTB0A2.DINIC-VGCIA-TARIF
                     AND
                     DFIM_VGCIA_TARIF   >= :GFCTB0A2.DINIC-VGCIA-TARIF
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'PARM_SERVC_TARIF'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0151'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE 'A2' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.

           MOVE FD-CSERVC-TARIF   TO
                CSERVC-TARIF OF GFCTB0G3.
           MOVE DINIC-VGCIA-TARIF OF GFCTB0A2 TO
                DINIC-VGCIA-TARIF OF GFCTB0G3.

           EXEC SQL
                SELECT VCOMPO_TARIF
                INTO
                  :GFCTB0G3.VCOMPO-TARIF
                FROM DB2PRD.VLR_COMPO_TARIF
                WHERE
                    CSERVC_TARIF = :GFCTB0G3.CSERVC-TARIF
                AND DINIC_VGCIA_TARIF = :GFCTB0G3.DINIC-VGCIA-TARIF
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'VLR_COMPO_TARIF'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0191'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE 'G3' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.

           MOVE VCOMPO-TARIF OF GFCTB0G3 TO WRK-CAMPO-VALOR-SINAL.
           MOVE '+'                      TO WRK-CAMPO-V-SINAL-S.
           MOVE WRK-CAMPO-V-SINAL-X      TO WBTA-VCOMPO-TARIF.

           MOVE FD-CTARIF-PCOTE       TO
                CTARIF-PCOTE OF GFCTB029.
           MOVE FD-CSERVC-TARIF       TO
                CTARIF-COMPO-PCOTE OF GFCTB029.
           MOVE GFCT06-DPROCM-ATUAL TO
                DINIC-VGCIA-COMPS OF GFCTB029
                DFIM-VGCIA-COMPS OF GFCTB029.
______*____MOVE_CSEQ-MANUT-SERVC_OF_GFCTB0A2_TO
______*_________CSEQ-MANUT-SERVC_OF_GFCTB029.

           EXEC SQL
                SELECT
                   DINIC_VGCIA_COMPS,
                   QFRANQ_COMPO_PCOTE,
                   VEXCED_COMPO_PCOTE,
                   CIDTFD_VLR_EXCED,
                   CSEQ_COMPO_PCOTE
                INTO
                  :GFCTB029.DINIC-VGCIA-COMPS,
                  :GFCTB029.QFRANQ-COMPO-PCOTE,
                  :GFCTB029.VEXCED-COMPO-PCOTE,
                  :GFCTB029.CIDTFD-VLR-EXCED,
                  :GFCTB029.CSEQ-COMPO-PCOTE
                FROM DB2PRD.COMPS_TARIF_PCOTE
                WHERE CTARIF_PCOTE       = :GFCTB029.CTARIF-PCOTE
                AND   CTARIF_COMPO_PCOTE = :GFCTB029.CTARIF-COMPO-PCOTE
                AND   DINIC_VGCIA_COMPS <= :GFCTB029.DINIC-VGCIA-COMPS
                AND   DFIM_VGCIA_COMPS >= :GFCTB029.DFIM-VGCIA-COMPS
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'COMPS_TARIF_PCOTE'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0191'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE '29' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.

           MOVE 'N' TO WRK-CESTA-PERSON-OK.

           IF CTPO-SERVC-TARIF OF GFCTB009 EQUAL 4
              MOVE '1' TO WRK-CESTA-PERSON-STA
              PERFORM 3500-VERIFICA-CESTA-PERSON
           ELSE
              MOVE 'N' TO WRK-CESTA-PERSON-STA
           END-IF.

           IF WRK-CESTA-PERSON-OK EQUAL 'S'
              MOVE QFRANQ-COMPO-PERSO OF GFCTB0O2 TO
                   WRK-CAMPO-TIRA-SINAL
           ELSE
              MOVE QFRANQ-COMPO-PCOTE OF GFCTB029 TO
                   WRK-CAMPO-TIRA-SINAL
           END-IF.

           MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
           MOVE WRK-CAMPO-TIRA-SINAL-03 TO WBTA-QFRANQ-COMPO-PCOTE.

           IF CIDTFD-VLR-EXCED OF GFCTB029 EQUAL 1
              MOVE VCOMPO-TARIF OF GFCTB0G3 TO WRK-CAMPO-VALOR-SINAL
              MOVE '+'                      TO WRK-CAMPO-V-SINAL-S
              MOVE WRK-CAMPO-V-SINAL-X      TO WBTA-VEXCED-COMPO-PCOTE
           ELSE
              MOVE VEXCED-COMPO-PCOTE OF GFCTB029 TO
                   WRK-CAMPO-VALOR-SINAL
              MOVE '+'                      TO WRK-CAMPO-V-SINAL-S
              MOVE WRK-CAMPO-V-SINAL-X      TO WBTA-VEXCED-COMPO-PCOTE
           END-IF.

           MOVE CJUNC-DEPDC OF GFCTB009 TO
                CJUNC-DEPDC OF CLIEV008.
           MOVE CCTA-CLI    OF GFCTB009 TO
                CCTA-CLI    OF CLIEV008

           EXEC SQL
                SELECT CID_CLI
                INTO
                  :CLIEV008.CID-CLI
                FROM DB2PRD.V01CTA_CORRENTE
                WHERE CJUNC_DEPDC        = :CLIEV008.CJUNC-DEPDC
                AND   CCTA_CLI           = :CLIEV008.CCTA-CLI
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'V01CTA_CORRENTE'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0201'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE 'C8' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.

           MOVE CID-CLI OF CLIEV008 TO
                CID-CLI OF CLIEV004.

           EXEC SQL
                SELECT CCGC_CPF,
                       CFLIAL_CGC,
                       CCTRL_CPF_CGC,
ST25X6                 CCGC_CPF_ST,
ST25X6                 CFLIAL_CGC_ST,
ST25X6                 CCTRL_CPF_CGC_ST
                INTO
                  :CLIEV004.CCGC-CPF,
                  :CLIEV004.CFLIAL-CGC,
                  :CLIEV004.CCTRL-CPF-CGC,
ST25X6            :CLIEV004.CCGC-CPF-ST,
ST25X6            :CLIEV004.CFLIAL-CGC-ST,
ST25X6            :CLIEV004.CCTRL-CPF-CGC-ST
                FROM DB2PRD.V01CLIENTE_AGENCIA
                WHERE CID_CLI            = :CLIEV004.CID-CLI
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'              TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'V01CLIENTE_AGENCIA'   TO  ERR-DBD-TAB
                MOVE 'SELECT '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0202'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL + 100
              MOVE 'C4' TO WBTA-CD-RETORNO
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1   TO  ACU-GRAVA-SAIDA-ERR
              GO TO 2520-99-FIM
           END-IF.


ST25X6* MOVIMENTACOES ANTIGAS VIA CAMPO NUMERICO COM SINAL
ST25X6*    MOVE CCGC-CPF OF CLIEV004 TO WRK-CAMPO-TIRA-SINAL.
ST25X6*    MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
ST25X6*    MOVE WRK-CAMPO-TIRA-SINAL-09 TO WBTA-CNPJ-PRINC.

ST25X6*    MOVE CFLIAL-CGC OF CLIEV004 TO WRK-CAMPO-TIRA-SINAL.
ST25X6*                                        
ST25X6*    MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
ST25X6*    MOVE WRK-CAMPO-TIRA-SINAL-04 TO WBTA-CNPJ-FILIAL.

ST25X6*    MOVE CCTRL-CPF-CGC OF CLIEV004 TO WRK-CAMPO-TIRA-SINAL.
ST25X6*    MOVE '+' TO WRK-CAMPO-TIRA-SINAL-S
ST25X6*    MOVE WRK-CAMPO-TIRA-SINAL-02 TO WBTA-CNPJ-CONTR.

ST25X6*  MOVIMENTACOES DIRETAS PARA CAMPOS DE CNPJ/FILIAL/CONTR
ST25X6     MOVE CCGC-CPF-ST    OF CLIEV004 TO WBTA-CNPJ-PRINC.
ST25X6     MOVE CFLIAL-CGC-ST  OF CLIEV004 TO WBTA-CNPJ-FILIAL.
ST25X6     MOVE CCTRL-CPF-CGC-ST OF CLIEV004 TO WBTA-CNPJ-CONTR.

           IF WRK-CESTA-PERSON-STA EQUAL 'N' OR
              WRK-CESTA-PERSON-OK EQUAL 'S'
              WRITE GFCTWBTA-REG
              MOVE    WRK-GRAVACAO TO  WRK-OPERACAO
              PERFORM 1300-TESTAR-FS-SAIDA
              ADD 1                TO  ACU-GRAVA-SAIDA
           END-IF.


      *---------------------------------------------------------------*
       2520-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
       2530-FECHAR-CURSOR-009 SECTION.
      *---------------------------------------------------------------*

           EXEC SQL
                CLOSE CSR01-GFCTB009
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'CLOSE'               TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'ADSAO_INDVD_PCOTE'   TO  ERR-DBD-TAB
                MOVE 'CLOSE '             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0031'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       2530-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA PROCESSA
      *===============================================================*
       3000-PROCESSA SECTION.
      *---------------------------------------------------------------*

           PERFORM 2500-TRATA-CURSOR-009.

           PERFORM 2200-LER-ENTRADA.

      *---------------------------------------------------------------*
       3000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    VERIFICA FRANQUIA PARA CESTA PERSONALIZADA
      *===============================================================*
       3500-VERIFICA-CESTA-PERSON SECTION.

             MOVE CJUNC-DEPDC OF GFCTB009 TO
                  CDEPDC      OF GFCTB0O2.
             MOVE CCTA-CLI OF GFCTB009 TO
                  CCTA-CLI OF GFCTB0O2.
             MOVE CSERVC-TARIF OF GFCTB009 TO
                  CSERVC-TARIF OF GFCTB0O2
                  CTARIF-PCOTE OF GFCTB0O2.
             MOVE HINCL-REG OF GFCTB009 TO
                  HINCL-REG OF GFCTB0O2.
             MOVE DINIC-VGCIA-COMPS OF GFCTB029 TO
                  DINIC-VGCIA-COMPS OF GFCTB0O2.
             MOVE CSEQ-COMPO-PCOTE OF GFCTB029 TO
                  CSEQ-COMPO-PCOTE OF GFCTB0O2.

           EXEC SQL
               SELECT QFRANQ_COMPO_PERSO
               INTO  :GFCTB0O2.QFRANQ-COMPO-PERSO
               FROM   DB2PRD.TCOMPS_PCOTE_PERSO
               WHERE CDEPDC            = :GFCTB0O2.CDEPDC
               AND   CCTA_CLI          = :GFCTB0O2.CCTA-CLI
               AND   CSERVC_TARIF      = :GFCTB0O2.CSERVC-TARIF
               AND   CTARIF_PCOTE      = :GFCTB0O2.CTARIF-PCOTE
               AND   HINCL_REG         = :GFCTB0O2.HINCL-REG
               AND   DINIC_VGCIA_COMPS = :GFCTB0O2.DINIC-VGCIA-COMPS
               AND   CSEQ_COMPO_PCOTE  = :GFCTB0O2.CSEQ-COMPO-PCOTE
           END-EXEC.

           IF ( SQLCODE NOT EQUAL ZEROS AND +100  ) OR
              ( SQLWARN0    EQUAL 'W' )
                MOVE 'SELECT'               TO  WRK-COMANDO
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                MOVE 'TCOMPS_PCOTE_PERSO'   TO  ERR-DBD-TAB
                MOVE 'SELECT'             TO  ERR-FUN-COMANDO
                MOVE  SQLCODE             TO  ERR-SQL-CODE
                MOVE '0531'               TO  ERR-LOCAL
                MOVE  SPACES              TO  ERR-SEGM
                MOVE 'DB2'                TO  ERR-TIPO-ACESSO
                PERFORM 9999-ROTINA-ERRO
           END-IF.

           IF SQLCODE EQUAL ZEROS
              MOVE 'S' TO WRK-CESTA-PERSON-OK
           ELSE
              MOVE 'N' TO WRK-CESTA-PERSON-OK
           END-IF.


      *---------------------------------------------------------------*
       3500-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA FINAL
      *===============================================================*
       4000-ROTINA-FINAL   SECTION.
      *---------------------------------------------------------------*

           MOVE ACU-LIDOS-ARQDATAS     TO  WRK-MASK-LIDOS-ARQDATAS
           MOVE ACU-LIDOS-ENTRADA      TO  WRK-MASK-LIDOS-ENTRADA
           MOVE ACU-GRAVA-SAIDA        TO  WRK-MASK-GRAVA-SAIDA
           MOVE ACU-GRAVA-SAIDA-ERR    TO  WRK-MASK-GRAVA-SAIDA-ERR

           DISPLAY '*********************** GFCT4464 *******************
      -                                                        '****'.
           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    ARQDATAS... : '
                                       WRK-MASK-LIDOS-ARQDATAS ' *'.
           DISPLAY '* QTDE DE REGISTROS LIDOS    ENTRADA.... : '
                                       WRK-MASK-LIDOS-ENTRADA ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS SAIDA.OK... : '
                                       WRK-MASK-GRAVA-SAIDA   ' *'.
           DISPLAY '* QTDE DE REGISTROS GRAVADOS SAIDA.ERR.. : '
                                       WRK-MASK-GRAVA-SAIDA-ERR   ' *'.

           DISPLAY '*
      -                                                        '   *'.
           DISPLAY '*********************** GFCT4464 *******************
      -                                                        '****'.

      *---------------------------------------------------------------*
       4000-99-FIM. EXIT.
      *---------------------------------------------------------------*

      *===============================================================*
      *    ROTINA ERRO
      *===============================================================*
       9999-ROTINA-ERRO SECTION.
      *---------------------------------------------------------------*

           MOVE 'APL'                  TO  ERR-TIPO-ACESSO
           MOVE 'GFCT4464'             TO  ERR-PGM

           CALL 'POOL7100'          USING  WRK-BATCH
                                           ERRO-AREA
                                           SQLCA.

           GOBACK.

      *---------------------------------------------------------------*
       9999-99-FIM. EXIT.
      *---------------------------------------------------------------*
