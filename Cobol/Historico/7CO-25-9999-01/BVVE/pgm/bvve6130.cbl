      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE6130.
       AUTHOR.     DANILO AUGUSTO MOREIRA.
      *================================================================*
      *            C P M  B R A X I S  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....:     BVVE6130                                  *
      *    PROGRAMADOR.:     DANILO AUGUSTO MOREIRA  - CPMBRAXIS       *
      *    ANALISTA    :     DANILO AUGUSTO MOREIRA  - CPMBRAXIS       *
      *    DATA........:     07/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:     ACRESCENTA ENDERECOS DE ESTABELECIMENTOS  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME                         INCLUDE/BOOK               *
      *           AFILCLIE.CLASS              I#BVVEGE                 *
      *           BVVENB03.CLASS              I#BVVEGF                 *
      *           AFILENDE                    I#BVVEGH                 *
      *----------------------------------------------------------------*
      *    INC'S/BOOK'S:                                               *
      *      I#BVVEGE, I#BVVEGF, I#BVVEGH                              *
      *----------------------------------------------------------------*
      *    MODULOS.....:                    INCLUDE/BOOK               *
      *           BRAD7100                    I#BRAD7C                 *
      *================================================================*

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

             SELECT AFILCLAS ASSIGN    TO UT-S-AFILCLAS
                        FILE STATUS    IS WRK-FS-AFILCLAS.

             SELECT ENDECLAS ASSIGN    TO UT-S-ENDECLAS
                        FILE STATUS    IS WRK-FS-ENDECLAS.

             SELECT AFILENDE ASSIGN    TO UT-S-AFILENDE
                        FILE STATUS    IS WRK-FS-AFILENDE.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * INPUT                                                          *
      *----------------------------------------------------------------*

       FD  AFILCLAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGE'.

      *----------------------------------------------------------------*
      * INPUT                                                          *
      *----------------------------------------------------------------*

       FD  ENDECLAS
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGF'.

      *----------------------------------------------------------------*
      * OUTPUT                                                         *
      *----------------------------------------------------------------*

       FD  AFILENDE
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGH'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77  WRK-IND                     PIC  9(003) COMP-3  VALUE ZEROS.
       77  ACU-GRAVADOS                PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-R                 PIC  9(009) COMP-3  VALUE ZEROS.
       77  ACU-LIDOS-A                 PIC  9(009) COMP-3  VALUE ZEROS.
       77  WRK-GRAVOU                  PIC  X(001)         VALUE SPACES.
       77  WRK-ENDERECOS               PIC  9(001)         VALUE ZEROS.

       01  WRK-CHAVE-BVVEGE.
7C2511     10  WRK-BVVEGE-CCNPJ-CPF-ESTBL
                                       PIC X(009)          VALUE SPACES.
7C2511*    10  WRK-BVVEGE-CCNPJ-CPF-ESTBL
7C2511*                                    PIC 9(009)          VALUE ZEROS.
7C2511     10  WRK-BVVEGE-CFLIAL-CNPJ-ESTBL
                                       PIC X(004)          VALUE SPACES.
7C2511*    10  WRK-BVVEGE-CFLIAL-CNPJ-ESTBL
7C2511*                                    PIC 9(005)          VALUE ZEROS.
       01  WRK-CHAVE-BVVEGF.
7C2511     10  WRK-BVVEGF-CCNPJ-CPF-ESTBL
                                       PIC X(009)          VALUE SPACES.
7C2511*    10  WRK-BVVEGF-CCNPJ-CPF-ESTBL
7C2511*                                    PIC 9(009)          VALUE ZEROS.
7C2511     10  WRK-BVVEGF-CFLIAL-CNPJ-ESTBL
                                       PIC X(004)          VALUE SPACES.
7C2511*    10  WRK-BVVEGF-CFLIAL-CNPJ-ESTBL
7C2511*                                    PIC 9(005)          VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES E FILE STATUS ***'.
      *----------------------------------------------------------------*

       01  WRK-FIM-AFILCLAS            PIC  X(001)         VALUE SPACES.
       01  WRK-FIM-ENDECLAS            PIC  X(001)         VALUE SPACES.

       01  WRK-FS-ARQUIVOS.
           05  WRK-FS-AFILCLAS         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-ENDECLAS         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-AFILENDE         PIC  X(002)         VALUE SPACES.

       01  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA'.
       01  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       01  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO'.
       01  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.
       01  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE MENSAGEM DE ERRO DO FILE STATUS ***'.
      *----------------------------------------------------------------*

       01 WRK-ERRO-TEXTO.
          05  FILLER                   PIC  X(008)         VALUE
              '** ERRO '.
          05  WRK-OPERACAO             PIC  X(013)         VALUE SPACES.
          05  FILLER                   PIC  X(012)         VALUE
              ' DO ARQUIVO '.
          05  WRK-ARQUIVO              PIC  X(008)         VALUE SPACES.
          05  FILLER                   PIC  X(017)         VALUE
              ' - FILE STATUS = '.
          05  WRK-FILE-STATUS          PIC  X(002)         VALUE SPACES.
          05  FILLER                   PIC  X(003)         VALUE ' **'.
          05  FILLER                   PIC  X(012)         VALUE SPACES.

           COPY 'I#BRAD7C'.

      *---------------------------------------------------------------*
      *  OS CPOS ABAIXO DEVEM SER INICIALIZADOS SO' QDO O APLICATIVO  *
      *  USAR O SISTEMA DE SENHAS (BRAD1000).                         *
      *---------------------------------------------------------------*
           05  WRK-ERR-DADOS-SENHAS.

               07  WRK-ERR-COD-USER    PIC  X(07).

               07  WRK-ERR-COD-DEPTO   PIC  X(06).

               07  WRK-ERR-MODULO      PIC  X(08)  VALUE SPACES.


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*


      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZACAO.

           PERFORM 2000-PROCESSAR
             UNTIL WRK-FIM-AFILCLAS    EQUAL  'S'
           PERFORM 3000-FINALIZACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZACAO              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  AFILCLAS
                       ENDECLAS
                OUTPUT AFILENDE.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS.
           PERFORM 1300-VERIFICA-VAZIO.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-AFILCLAS.
           PERFORM 1120-TESTAR-FS-ENDECLAS.
           PERFORM 1130-TESTAR-FS-AFILENDE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-AFILCLAS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AFILCLAS         NOT EQUAL '00'
               MOVE WRK-FS-AFILCLAS    TO  WRK-FILE-STATUS
               MOVE 'AFILCLAS'         TO  WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO  ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-ENDECLAS         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-ENDECLAS         NOT EQUAL '00'
               MOVE WRK-FS-ENDECLAS    TO WRK-FILE-STATUS
               MOVE 'ENDECLAS'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-AFILENDE         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AFILENDE         NOT EQUAL '00'
               MOVE WRK-FS-AFILENDE    TO WRK-FILE-STATUS
               MOVE 'AFILENDE'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           PERFORM 1400-LER-AFILCLAS.

           IF  WRK-FIM-AFILCLAS        EQUAL 'S'
               DISPLAY '************** B V V E 6 1 3 0 ************'
               DISPLAY '*         ARQUIVO AFILCLAS  VAZIO         *'
               DISPLAY '************** B V V E 6 1 3 0 ************'
           END-IF.

           PERFORM 1500-LER-ENDECLAS.

           IF  WRK-FIM-ENDECLAS        EQUAL 'S'
               DISPLAY '************** B V V E 6 1 3 0 ************'
               DISPLAY '*         ARQUIVO  ENDECLAS  VAZIO        *'
               DISPLAY '************** B V V E 6 1 3 0 ************'
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-LER-AFILCLAS               SECTION.
      *----------------------------------------------------------------*

           READ AFILCLAS
             AT END
                MOVE 'S'               TO WRK-FIM-AFILCLAS
                GO             TO 1400-99-FIM
            NOT AT END
                MOVE WRK-LEITURA       TO WRK-OPERACAO
                PERFORM 1110-TESTAR-FS-AFILCLAS
                ADD  1                 TO  ACU-LIDOS-R
           END-READ.

           IF   WRK-FIM-AFILCLAS NOT EQUAL 'S'
                MOVE 'N'               TO  WRK-GRAVOU
           END-IF.

           IF (BVVEGE-CCNPJ-CPF-ESTBL
                                   LESS WRK-BVVEGE-CCNPJ-CPF-ESTBL) OR
             ((BVVEGE-CCNPJ-CPF-ESTBL
                                   EQUAL WRK-BVVEGE-CCNPJ-CPF-ESTBL)AND
              (BVVEGE-CFLIAL-CNPJ-ESTBL
                                   LESS WRK-BVVEGE-CFLIAL-CNPJ-ESTBL))
               DISPLAY '************** B V V E 6 1 3 0 ************'
               DISPLAY '*    ARQUIVO  AFILCLIE DESCLASSIFICADO    *'
               DISPLAY '************** B V V E 6 1 3 0 ************'
               MOVE '*** ARQUIVO  AFILCLIE DESCLASSIFICADO ***'
                                   TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           ELSE
               MOVE BVVEGE-CCNPJ-CPF-ESTBL
                                       TO WRK-BVVEGE-CCNPJ-CPF-ESTBL
               MOVE BVVEGE-CFLIAL-CNPJ-ESTBL
                                       TO WRK-BVVEGE-CFLIAL-CNPJ-ESTBL
           END-IF.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-LER-ENDECLAS               SECTION.
      *----------------------------------------------------------------*

           READ ENDECLAS
             AT END
                MOVE 'S'               TO WRK-FIM-ENDECLAS
                MOVE HIGH-VALUES       TO WRK-CHAVE-BVVEGF
                GO                     TO 1500-99-FIM
            NOT AT END
                MOVE WRK-LEITURA       TO WRK-OPERACAO
                PERFORM 1120-TESTAR-FS-ENDECLAS
                ADD 1                  TO ACU-LIDOS-A
           END-READ.

           IF (BVVEGF-CCNPJ-CPF-ESTBL
                                   LESS WRK-BVVEGF-CCNPJ-CPF-ESTBL) OR
             ((BVVEGF-CCNPJ-CPF-ESTBL
                                   EQUAL WRK-BVVEGF-CCNPJ-CPF-ESTBL)AND
              (BVVEGF-CFLIAL-CNPJ-ESTBL
                                   LESS WRK-BVVEGF-CFLIAL-CNPJ-ESTBL))
               DISPLAY '************** B V V E 6 1 3 0 ************'
               DISPLAY '*    ARQUIVO  ENDECLAS DESCLASSIFICADO    *'
               DISPLAY '************** B V V E 6 1 3 0 ************'
               MOVE '*** ARQUIVO  BVVEBN03 DESCLASSIFICADO ***'
                                   TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           ELSE
               MOVE BVVEGF-CCNPJ-CPF-ESTBL
                                       TO WRK-BVVEGF-CCNPJ-CPF-ESTBL
               MOVE BVVEGF-CFLIAL-CNPJ-ESTBL
                                       TO WRK-BVVEGF-CFLIAL-CNPJ-ESTBL
           END-IF.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           MOVE                        1 TO WRK-ENDERECOS

           IF  WRK-CHAVE-BVVEGE        EQUAL WRK-CHAVE-BVVEGF
               PERFORM UNTIL (WRK-CHAVE-BVVEGE
                                       NOT EQUAL WRK-CHAVE-BVVEGF)
                   PERFORM 2100-TRATA-IGUAL
                   PERFORM 1500-LER-ENDECLAS
                   ADD 1               TO WRK-ENDERECOS
               END-PERFORM
               PERFORM 2400-GRAVAR-AFILENDE
               PERFORM 1400-LER-AFILCLAS
           ELSE
               IF  WRK-CHAVE-BVVEGE    GREATER WRK-CHAVE-BVVEGF
                   PERFORM 1500-LER-ENDECLAS
              ELSE
                   DISPLAY '************** B V V E 6 1 3 0 ************'
                   DISPLAY '*    ENDERECO NAO ENCONTRADO ENDECLAS     *'
                   DISPLAY '************** B V V E 6 1 3 0 ************'
                   MOVE '*** ENDERECO NAO ENCONTRADO ENDECLAS  ***'
                                      TO ERR-TEXTO
                   PERFORM 9999-TRATAR-ERRO
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-TRATA-IGUAL                SECTION.
      *----------------------------------------------------------------*

           IF  BVVEGF-CTPO-ENDER-ESTBL EQUAL 'C'
               MOVE 1                  TO WRK-IND
           ELSE
               MOVE 2                  TO WRK-IND
           END-IF.

           MOVE BVVEGF-ELOGDR-ESTBL-COML
                                   TO BVVEGH-ELOGDR-ESTBL-COML(WRK-IND).
           MOVE BVVEGF-ELOGDR-NRO-ESTBL
                                   TO BVVEGH-ELOGDR-NRO-ESTBL(WRK-IND).

           IF   BVVEGF-ECOMPL-ESTBL-COML-N EQUAL '?'
                MOVE   SPACES      TO BVVEGH-ECOMPL-ESTBL-COML(WRK-IND)
           ELSE
                MOVE BVVEGF-ECOMPL-ESTBL-COML
                                   TO BVVEGH-ECOMPL-ESTBL-COML(WRK-IND)
           END-IF.

           MOVE BVVEGF-EBAIRO-ESTBL-COML
                                   TO BVVEGH-EBAIRO-ESTBL-COML(WRK-IND).
           MOVE BVVEGF-ECIDDE-ESTBL-COML
                                   TO BVVEGH-ECIDDE-ESTBL-COML(WRK-IND).
           MOVE BVVEGF-CSGL-UF
                                   TO BVVEGH-CSGL-UF(WRK-IND).
           MOVE BVVEGF-CCEP-COMPL
                                   TO BVVEGH-CCEP-COMPL(WRK-IND).
           MOVE BVVEGF-CCEP
                                   TO BVVEGH-CCEP(WRK-IND).

           IF  WRK-ENDERECOS       EQUAL 1
           MOVE BVVEGE-CCNPJ-CPF-ESTBL TO BVVEGH-CCNPJ-CPF-ESTBL
           MOVE BVVEGE-CFLIAL-CNPJ-ESTBL
                                       TO BVVEGH-CFLIAL-CNPJ-ESTBL
           MOVE BVVEGE-CCTRL-CNPJ-CPF  TO BVVEGH-CCTRL-CNPJ-CPF
           MOVE BVVEGE-IESTBL-AFLIA    TO BVVEGH-IESTBL-AFLIA
           MOVE BVVEGE-IPLAQT-EQPMT-ESTBL
                                       TO BVVEGH-IPLAQT-EQPMT-ESTBL
           MOVE BVVEGE-CBCO            TO BVVEGH-CBCO
           MOVE BVVEGE-CAG-BCRIA       TO BVVEGH-CAG-BCRIA
           MOVE BVVEGE-CCTA-BCRIA-CLI  TO BVVEGH-CCTA-BCRIA-CLI
           MOVE BVVEGE-CSIT-PPSTA-AFLIA
                                       TO BVVEGH-CSIT-PPSTA-AFLIA
           MOVE BVVEGE-CCNPJ-CPF-CONTT TO BVVEGH-CCNPJ-CPF-CONTT
           MOVE BVVEGE-CFLIAL-CNPJ-CONTT
                                       TO BVVEGH-CFLIAL-CNPJ-CONTT
           MOVE BVVEGE-CCTRL-CNPJ-CONTT
                                       TO BVVEGH-CCTRL-CNPJ-CONTT
           MOVE BVVEGE-CRAMO-ATVDD-ESTBL
                                       TO BVVEGH-CRAMO-ATVDD-ESTBL
           MOVE BVVEGE-CCRM-RESP-ESTBL TO BVVEGH-CCRM-RESP-ESTBL
           MOVE BVVEGE-CCRO-RESP-ESTBL TO BVVEGH-CCRO-RESP-ESTBL
           MOVE BVVEGE-ICNTAT-ESTBL-AFLIA
                                       TO BVVEGH-ICNTAT-ESTBL-AFLIA
           MOVE BVVEGE-CENDER-COML-IGUAL
                                       TO BVVEGH-CENDER-COML-IGUAL
           MOVE BVVEGE-CDDD-FONE-ESTBL TO BVVEGH-CDDD-FONE-ESTBL
           MOVE BVVEGE-CFONE-ESTBL-COML
                                       TO BVVEGH-CFONE-ESTBL-COML
           MOVE BVVEGE-CDDD-FAX-ESTBL  TO BVVEGH-CDDD-FAX-ESTBL
           MOVE BVVEGE-CFONE-FAX-ESTBL TO BVVEGH-CFONE-FAX-ESTBL
           MOVE BVVEGE-EEMAIL-CNTAT-ESTBL
                                       TO BVVEGH-EEMAIL-CNTAT-ESTBL
           MOVE BVVEGE-CTPO-FUNCN-ESTBL
                                       TO BVVEGH-CTPO-FUNCN-ESTBL
           MOVE BVVEGE-CTPO-ESTBL-COML TO BVVEGH-CTPO-ESTBL-COML
           MOVE BVVEGE-CPTO-VDA-MTRIZ  TO BVVEGH-CPTO-VDA-MTRIZ
           MOVE BVVEGE-CPTO-VDA-CTLZA  TO BVVEGH-CPTO-VDA-CTLZA
           MOVE BVVEGE-CLOC-PGTO-ESTBL TO BVVEGH-CLOC-PGTO-ESTBL
           MOVE BVVEGE-CLOC-ESTBL-COML TO BVVEGH-CLOC-ESTBL-COML
           MOVE BVVEGE-DINCL-ESTBL-COML
                                       TO BVVEGH-DINCL-ESTBL-COML
           MOVE BVVEGE-CEMPR-INC       TO BVVEGH-CEMPR-INC
           MOVE BVVEGE-CDEPDC          TO BVVEGH-CDEPDC
           MOVE BVVEGE-CFUNC-BDSCO     TO BVVEGH-CFUNC-BDSCO
           MOVE BVVEGE-CFUNC-ALT-ESTBL TO BVVEGH-CFUNC-ALT-ESTBL
           MOVE BVVEGE-DALT-ESTBL-COML TO BVVEGH-DALT-ESTBL-COML
           MOVE BVVEGE-PTX-PCELD-JURO  TO BVVEGH-PTX-PCELD-JURO
           MOVE BVVEGE-PTX-CREDT-ROTVO TO BVVEGH-PTX-CREDT-ROTVO
           MOVE BVVEGE-PTX-CREDT-PCELD TO BVVEGH-PTX-CREDT-PCELD
           MOVE BVVEGE-PTX-VDA-INTL    TO BVVEGH-PTX-VDA-INTL
           MOVE BVVEGE-PTX-CATAO-DEB   TO BVVEGH-PTX-CATAO-DEB
           MOVE BVVEGE-RAZAO-SOCIAL    TO BVVEGH-RAZAO-SOCIAL
           MOVE BVVEGE-DATA-FUNDACAO   TO BVVEGH-DATA-FUNDACAO
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2105-CHECA-COMP-END             SECTION.
      *----------------------------------------------------------------*

           IF   BVVEGF-ECOMPL-ESTBL-COML-N EQUAL '?'
                MOVE   SPACES      TO BVVEGH-ECOMPL-ESTBL-COML(WRK-IND)
           ELSE
                MOVE BVVEGF-ECOMPL-ESTBL-COML
                                   TO BVVEGH-ECOMPL-ESTBL-COML(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       2105-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2400-GRAVAR-AFILENDE            SECTION.
      *----------------------------------------------------------------*

           WRITE   BVVEGH-REG          OF AFILENDE.
           MOVE    'S'                 TO WRK-GRAVOU
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILENDE.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZACAO                SECTION.
      *----------------------------------------------------------------*

           CLOSE AFILCLAS
                 ENDECLAS
                 AFILENDE.

           MOVE    WRK-FECHAMENTO           TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           DISPLAY '***************** B V V E 6 1 3 0 ****************'.
           DISPLAY '*         RESUMO DO PROCESSAMENTO                *'.
           DISPLAY '* TOTAL DE REGS.LIDOS AFIL. PEND. C/ DADOS DO CL *'.
           DISPLAY '*              ' ACU-LIDOS-R '       *'.
           DISPLAY '* TOTAL DE REGS.LIDOS ENDERECOS DE ESTABEL.:     *'.
           DISPLAY '*              ' ACU-LIDOS-A '       *'.
           DISPLAY '* TOTAL DE REGS.GRAV. AFIL.PEND. C/ TX. E END.  :*'.
           DISPLAY '*              ' ACU-GRAVADOS '       *'.
           DISPLAY '***************** B V V E 6 1 3 0 ****************'.

           STOP RUN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-TRATAR-ERRO            SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE6130'           TO ERR-PGM
           MOVE 'APL'                TO ERR-TIPO-ACESSO
           MOVE SPACES               TO WRK-ERR-DADOS-SENHAS

           CALL 'BRAD7100' USING WRK-BATCH
                                 ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
