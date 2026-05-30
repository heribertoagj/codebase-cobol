      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. BVVEA050
       AUTHOR.     MILTON.
      *================================================================*
      *
      *================================================================*
      *                         W I P R O  S / A                       *
      *================================================================*
      *    PROGRAMA....:   BVVEA050                                    *
      *    PROGRAMADOR.:   MILTON                    - WIPRO           *
      *    DATA........:   OUTUBRO/2020                                *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   RECEBER ARQUIVO DO OPTIN E SEPARAR OS CAMPOS*
      *                    PARA PROCESSAMENTO NO BVVE                  *
      *  --------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *                DDNAME           I/0           INCLUDE/BOOK     *
      *                ARQCIPEN         INPUT              -           *
      *                ARQCIPSS         OUTPUT          I#BVVEDK       *
      *----------------------------------------------------------------*
      *    BOOKS .:                                                    *
      *    I#BRAD7C - AREA PARA TRATAMENTO DE ERROS PELA BRAD7100      *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD7100 - TRATAMENTO DE ERRO                               *
      *=============================================================== *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT ARQCIPEN ASSIGN      TO UT-S-ARQCIPEN
                    FILE STATUS        IS WRK-FS-ARQCIPEN.

           SELECT ARQCIPSS ASSIGN      TO UT-S-ARQCIPSS
                    FILE STATUS        IS WRK-FS-ARQCIPSS.


      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO ARQCIPEN                                    *
      *            ORG. SEQUENCIAL - LRECL = 200                       *
      *----------------------------------------------------------------*

       FD  ARQCIPEN
           RECORDING  MODE IS V
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQCIPEN.
           05  FILLER               PIC  X(157).

      *----------------------------------------------------------------*
      *    INPUT - ARQUIVO SAIDA ARQCIPSS                              *
      *            ORG. FIXO - LRECL = 155                             *
      *----------------------------------------------------------------*

       FD  ARQCIPSS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-ARQCIPSS            PIC  X(155).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** BVVEA050 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ACUMULADORES ***'.
      *----------------------------------------------------------------*

       01  ACU-MULADORES.
           03  ACU-LID-ARQCIPEN        PIC  9(09) COMP-3   VALUE ZEROS.
           03  ACU-GRA-ARQCIPSS        PIC  9(09) COMP-3   VALUE ZEROS.
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS DE OPERACAO DE ARQUIVOS ***'.
      *----------------------------------------------------------------*
       77  WRK-FS-ARQCIPEN             PIC X(02)           VALUE SPACES.
       77  WRK-FS-ARQCIPSS             PIC X(02)           VALUE SPACES.

       01  WRK-ABERTURA                PIC X(13)  VALUE ' NA ABERTURA '.
       01  WRK-GRAVACAO                PIC X(13)  VALUE ' NA GRAVACAO '.
       01  WRK-FECHAMENTO              PIC X(13)  VALUE 'NO FECHAMENTO'.
       01  WRK-LEITURA                 PIC X(13)  VALUE ' NA LEITURA  '.

       01  WRK-FIM-ARQCIPEN            PIC X(01)  VALUE  'N'.
       01  WRK-BATCH                   PIC X(08)  VALUE  'BATCH'.
       01  WRK-MASCARA                 PIC ZZZ.ZZZ.Z99     VALUE ZEROS.
       01  WRK-QDT-NUM                 PIC 9(03)  COMP-3   VALUE ZEROS.
       01  IND-1                       PIC 9(03)  COMP-3   VALUE ZEROS.

       01  WRK-ID-TR-NUM               PIC X(20)           VALUE SPACES.
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-01             PIC 9(01).
           05 FILLER                   PIC X(19).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-02             PIC 9(02).
           05 FILLER                   PIC X(18).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-03             PIC 9(03).
           05 FILLER                   PIC X(17).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-04             PIC 9(04).
           05 FILLER                   PIC X(16).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-05             PIC 9(05).
           05 FILLER                   PIC X(15).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-06             PIC 9(06).
           05 FILLER                   PIC X(14).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-07             PIC 9(07).
           05 FILLER                   PIC X(13).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-08             PIC 9(08).
           05 FILLER                   PIC X(12).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-09             PIC 9(09).
           05 FILLER                   PIC X(11).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-10             PIC 9(10).
           05 FILLER                   PIC X(10).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-11             PIC 9(11).
           05 FILLER                   PIC X(09).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-12             PIC 9(12).
           05 FILLER                   PIC X(08).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-13             PIC 9(13).
           05 FILLER                   PIC X(07).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-14             PIC 9(14).
           05 FILLER                   PIC X(06).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-15             PIC 9(15).
           05 FILLER                   PIC X(05).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-16             PIC 9(16).
           05 FILLER                   PIC X(04).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-17             PIC 9(17).
           05 FILLER                   PIC X(03).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-18             PIC 9(18).
           05 FILLER                   PIC X(02).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-19             PIC 9(19).
           05 FILLER                   PIC X(01).
       01  FILLER                REDEFINES WRK-ID-TR-NUM.
           05 WRK-ID-TR-20             PIC 9(20).

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   AREA MENSAGENS DE ERRO     *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-ERRO.
          03  FILLER                   PIC X(07)   VALUE SPACES.
          03  FILLER                   PIC X(07)   VALUE
              '* ERRO '.
          03  WRK-OPERACAO             PIC X(13)   VALUE SPACES.
          03  FILLER                   PIC X(12)   VALUE
              ' DO ARQUIVO '.
          03  WRK-NOME-ARQ             PIC X(08)   VALUE SPACES.
          03  FILLER                   PIC X(17)   VALUE
              ' - FILE-STATUS = '.
          03  WRK-FILE-STATUS          PIC X(02)   VALUE SPACES.
          03  FILLER                   PIC X(02)   VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36)         VALUE
           '* LAYOUT ARQUIVO ARQCIPSS *'.
      *----------------------------------------------------------------*
          COPY 'I#BVVEFK'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(36)         VALUE
           '* LAYOUT GENERICO ARQ. DA CIP *'.
      *----------------------------------------------------------------*
       01  WRK-REG-CIP-FD                      PIC X(200) VALUE SPACES.
       01  WRK-REG-CIP.
           05 WRK-ID-PRINC                     PIC X(014) VALUE SPACES.
           05 WRK-ID-ADM                       PIC X(014) VALUE SPACES.
           05 WRK-ID-OPTIN                     PIC X(019) VALUE SPACES.
           05 WRK-CNPJ-CPF-RECEB               PIC X(014) VALUE SPACES.
           05 WRK-CNPJ-CREDENC                 PIC X(014) VALUE SPACES.
           05 WRK-CNPJ-FINANC                  PIC X(014) VALUE SPACES.
           05 WRK-ARRANJO-PGTO                 PIC X(003) VALUE SPACES.
           05 WRK-DT-ASS-OPT                   PIC X(010) VALUE SPACES.
           05 WRK-DT-INI-OPT                   PIC X(010) VALUE SPACES.
           05 WRK-ID-DOMICILIO                 PIC X(001) VALUE SPACES.
           05 WRK-ID-TR                        PIC X(020) VALUE SPACES.
           05 WRK-CNPJ-CPF-OPT                 PIC X(014) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)          VALUE
           '*  AREA DA POOL7100  *'.
      *----------------------------------------------------------------*

           COPY  'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC X(32)           VALUE
           '*   FIM DA WORKING BVVEA050    *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-VER-VAZIO.

           PERFORM 3000-PROCESSAR      UNTIL
                   WRK-FIM-ARQCIPEN   EQUAL 'S'.

           PERFORM 4000-FINALIZAR.

           GOBACK.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA           SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT  ARQCIPEN
               OUTPUT  ARQCIPSS.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1200-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1210-TESTAR-FS-ARQCIPEN.

           PERFORM 1220-TESTAR-FS-ARQCIPSS.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1210-TESTAR-FS-ARQCIPEN         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQCIPEN         NOT EQUAL '00' AND '04'
               MOVE 'ARQCIPEN'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQCIPEN    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1220-TESTAR-FS-ARQCIPSS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ARQCIPSS         NOT EQUAL '00'
               MOVE 'ARQCIPSS'         TO WRK-NOME-ARQ
               MOVE WRK-FS-ARQCIPSS    TO WRK-FILE-STATUS
               MOVE WRK-MSG-ERRO       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VER-VAZIO                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-ARQCIPEN

           IF  WRK-FIM-ARQCIPEN  EQUAL 'S'
               DISPLAY   '***********BVVEA050******************* '
               DISPLAY   '*                                    * '
               DISPLAY   '*       ARQ    ARQCIPEN   VAZIO      * '
               DISPLAY   '*         PROGRAMA ENCERRADO         * '
               DISPLAY   '*  FILE STATUS  =  ' WRK-FS-ARQCIPEN
               DISPLAY   '************************************** '
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-ARQCIPEN               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO  WRK-REG-CIP-FD.

           READ ARQCIPEN               INTO WRK-REG-CIP-FD.

           IF  WRK-FS-ARQCIPEN         EQUAL '10'
               MOVE 'S'                TO   WRK-FIM-ARQCIPEN
               GO TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1210-TESTAR-FS-ARQCIPEN.

           ADD     1                   TO ACU-LID-ARQCIPEN.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           UNSTRING FD-REG-ARQCIPEN
             DELIMITED BY ';'
                  INTO WRK-ID-PRINC
                       WRK-ID-ADM
                       WRK-ID-OPTIN
                       WRK-CNPJ-CPF-RECEB
                       WRK-CNPJ-CREDENC
                       WRK-CNPJ-FINANC
                       WRK-ARRANJO-PGTO
                       WRK-DT-ASS-OPT
                       WRK-DT-INI-OPT
                       WRK-ID-DOMICILIO
                       WRK-ID-TR
                       WRK-CNPJ-CPF-OPT
           END-UNSTRING

           PERFORM 3100-MONTA-SAIDA

           PERFORM 3200-GRAVAR-ARQCIPSS

           PERFORM 2100-LER-ARQCIPEN
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-MONTA-SAIDA                SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEFK-REGISTRO

           MOVE WRK-ID-PRINC           TO BVVEFK-ID-PRINC
           MOVE WRK-ID-ADM             TO BVVEFK-ID-ADM
           MOVE WRK-ID-OPTIN           TO BVVEFK-ID-OPTIN
           MOVE WRK-CNPJ-CPF-RECEB     TO BVVEFK-CNPJ-CPF-RECEB
           MOVE WRK-CNPJ-CREDENC       TO BVVEFK-CNPJ-CREDENC
           MOVE WRK-CNPJ-FINANC        TO BVVEFK-CNPJ-FINANC
           MOVE WRK-ARRANJO-PGTO       TO BVVEFK-ARRANJO-PGTO
           MOVE WRK-ID-DOMICILIO       TO BVVEFK-ID-DOMICILIO
           MOVE WRK-CNPJ-CPF-OPT       TO BVVEFK-CNPJ-CPF-OPT

           STRING WRK-DT-ASS-OPT(9:2) '.'
                  WRK-DT-ASS-OPT(6:2) '.'
                  WRK-DT-ASS-OPT(1:4)
                  DELIMITED BY SIZE  INTO BVVEFK-DT-ASS-OPT
           END-STRING

           STRING WRK-DT-INI-OPT(9:2) '.'
                  WRK-DT-INI-OPT(6:2) '.'
                  WRK-DT-INI-OPT(1:4)
                  DELIMITED BY SIZE  INTO BVVEFK-DT-INI-OPT
           END-STRING

           MOVE '31.12.2099'           TO BVVEFK-DT-FIM-OPT

           PERFORM 3110-MONTA-NUMERICO
           .
      *----------------------------------------------------------------*
       3100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-MONTA-NUMERICO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-ID-TR              TO WRK-ID-TR-NUM
           MOVE ZEROS                  TO WRK-QDT-NUM
           PERFORM VARYING IND-1 FROM 1 BY 1
                     UNTIL WRK-ID-TR(IND-1:1) EQUAL SPACES
               ADD 1                   TO WRK-QDT-NUM
           END-PERFORM.

           EVALUATE WRK-QDT-NUM
               WHEN 1
                    MOVE WRK-ID-TR-01  TO BVVEFK-ID-TR
               WHEN 2
                    MOVE WRK-ID-TR-02  TO BVVEFK-ID-TR
               WHEN 3
                    MOVE WRK-ID-TR-03  TO BVVEFK-ID-TR
               WHEN 4
                    MOVE WRK-ID-TR-04  TO BVVEFK-ID-TR
               WHEN 5
                    MOVE WRK-ID-TR-05  TO BVVEFK-ID-TR
               WHEN 6
                    MOVE WRK-ID-TR-06  TO BVVEFK-ID-TR
               WHEN 7
                    MOVE WRK-ID-TR-07  TO BVVEFK-ID-TR
               WHEN 8
                    MOVE WRK-ID-TR-08  TO BVVEFK-ID-TR
               WHEN 9
                    MOVE WRK-ID-TR-09  TO BVVEFK-ID-TR
               WHEN 10
                    MOVE WRK-ID-TR-10  TO BVVEFK-ID-TR
               WHEN 11
                    MOVE WRK-ID-TR-11  TO BVVEFK-ID-TR
               WHEN 12
                    MOVE WRK-ID-TR-12  TO BVVEFK-ID-TR
               WHEN 13
                    MOVE WRK-ID-TR-13  TO BVVEFK-ID-TR
               WHEN 14
                    MOVE WRK-ID-TR-14  TO BVVEFK-ID-TR
               WHEN 15
                    MOVE WRK-ID-TR-15  TO BVVEFK-ID-TR
               WHEN 16
                    MOVE WRK-ID-TR-16  TO BVVEFK-ID-TR
               WHEN 17
                    MOVE WRK-ID-TR-17  TO BVVEFK-ID-TR
               WHEN OTHER
                    MOVE WRK-ID-TR-18  TO BVVEFK-ID-TR
           END-EVALUATE
           .
      *----------------------------------------------------------------*
       3110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-GRAVAR-ARQCIPSS            SECTION.
      *----------------------------------------------------------------*

           WRITE FD-REG-ARQCIPSS       FROM   BVVEFK-REGISTRO

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO

           PERFORM 1220-TESTAR-FS-ARQCIPSS

           ADD 1                       TO  ACU-GRA-ARQCIPSS.

      *----------------------------------------------------------------*
       3200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE ARQCIPEN
                 ARQCIPSS.

           MOVE WRK-FECHAMENTO         TO  WRK-OPERACAO.

           PERFORM 1200-TESTAR-FILE-STATUS.

           GOBACK.
      *----------------------------------------------------------------*
       4000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*************** BVVEA050 ******************'
           DISPLAY '*                                         *'

           MOVE ACU-LID-ARQCIPEN       TO WRK-MASCARA
           DISPLAY '*   REG. LIDOS EARQGRAN: ' WRK-MASCARA
                                                       '      *'

           MOVE ACU-GRA-ARQCIPSS       TO WRK-MASCARA
           DISPLAY '*   REG. GRAVA SGRAN022: ' WRK-MASCARA
                                                       '      *'

           DISPLAY '*                                         *'
           DISPLAY '*************** BVVEA045 ******************'.
           .
      *----------------------------------------------------------------*
       4100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO                SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVEA050'             TO ERR-PGM.

           CALL 'BRAD7100'         USING WRK-BATCH
                                         ERRO-AREA.

           GOBACK.
      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

