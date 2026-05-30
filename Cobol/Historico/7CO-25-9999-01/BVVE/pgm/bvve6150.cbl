      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. BVVE6150.
       AUTHOR.     DANILO AUGUSTO MOREIRA.
      *================================================================*
      *            C P M  B R A X I S  -  S I S T E M A S              *
      *----------------------------------------------------------------*
      *    PROGRAMA....:     BVVE6150                                  *
      *    PROGRAMADOR.:     DANILO AUGUSTO MOREIRA  - CPMBRAXIS       *
      *    ANALISTA    :     DANILO AUGUSTO MOREIRA  - CPMBRAXIS       *
      *    DATA........:     07/2008                                   *
      *----------------------------------------------------------------*
      *    OBJETIVO....:     FORMATA AFILIACOES PARA ENVIO A REDECARD  *
      *----------------------------------------------------------------*
      *    ARQUIVOS....:                                               *
      *      DDNAME                         INCLUDE/BOOK               *
      *           AFILCOMP                    I#BVVEGI                 *
      *           CTRLAFIL                    I#BVVEGJ                 *
      *           CTRLATUA                    I#BVVEGJ                 *
      *           AFILIENV                    I#BVVEGK                 *
      *           EATUFILI                    I#BVVEGL                 *
      *----------------------------------------------------------------*
      *    INC'S/BOOK'S:                                               *
      *      I#BVVEGI, I#BVVEGJ, I#BVVEGK, I#BVVEGL                    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                    INCLUDE/BOOK               *
      *           BRAD7100                    I#BRAD7C                 *
      *           POOL7600                                             *
      *           POOL0431                                             *
      *           POOL0110                                             *
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

             SELECT AFILCOMP ASSIGN    TO UT-S-AFILCOMP
                        FILE STATUS    IS WRK-FS-AFILCOMP.

             SELECT CTRLAFIL ASSIGN    TO UT-S-CTRLAFIL
                        FILE STATUS    IS WRK-FS-CTRLAFIL.

             SELECT CTRLATUA ASSIGN    TO UT-S-CTRLATUA
                        FILE STATUS    IS WRK-FS-CTRLATUA.

             SELECT AFILIENV ASSIGN    TO UT-S-AFILIENV
                        FILE STATUS    IS WRK-FS-AFILIENV.

             SELECT EATUFILI ASSIGN    TO UT-S-EATUFILI
                        FILE STATUS    IS WRK-FS-EATUFILI.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * INPUT                                                          *
      *----------------------------------------------------------------*

       FD  AFILCOMP
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGI'.

      *----------------------------------------------------------------*
      * INPUT/
      *----------------------------------------------------------------*

       FD  CTRLAFIL
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGJ'.

      *----------------------------------------------------------------*
      * OUTPUT
      *----------------------------------------------------------------*

       FD  CTRLATUA
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  REG-CTRLATUA.
           05  FILLER      PIC X(37).

      *----------------------------------------------------------------*
      * OUTPUT                                                         *
      *----------------------------------------------------------------*

       FD  AFILIENV
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGK'.

      *----------------------------------------------------------------*
      * OUTPUT                                                         *
      *----------------------------------------------------------------*

       FD  EATUFILI
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY 'I#BVVEGL'.

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

       77 ACU-GRAVADOS                 PIC  9(009) COMP-3  VALUE ZEROS.
       77 ACU-GRAVADOS-CTRLATUA        PIC  9(009) COMP-3  VALUE ZEROS.
       77 ACU-GRAVADOS-EATUFILI        PIC  9(009) COMP-3  VALUE ZEROS.
       77 ACU-LIDOS-R                  PIC  9(009) COMP-3  VALUE ZEROS.
       77 ACU-LIDOS-A                  PIC  9(009) COMP-3  VALUE ZEROS.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA TESTES E FILE STATUS ***'.
      *----------------------------------------------------------------*

       77  WRK-FIM-AFILCOMP            PIC  X(001)         VALUE SPACES.
       77  WRK-FIM-CTRLAFIL            PIC  X(001)         VALUE SPACES.
       77  WRK-IND-END                 PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-IND-PROP                PIC  9(003) COMP-3  VALUE ZEROS.
       77  WRK-IND-PROP-2              PIC  9(003) COMP-3  VALUE ZEROS.

       77  WRK-NSU                     PIC  9(006)         VALUE ZEROS.
       77  WRK-NRO-LOTE                PIC  9(006)         VALUE ZEROS.

       77  WRK-ABERTURA                PIC  X(013)         VALUE
           ' NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(013)         VALUE
           ' NA LEITURA'.
       77  WRK-GRAVACAO                PIC  X(013)         VALUE
           ' NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(013)         VALUE
           'NO FECHAMENTO'.
       77  WRK-BATCH                   PIC  X(008)         VALUE
           'BATCH'.

       01  WRK-REG-TRANSMISSAO.
           05  WRK-STRING              PIC  X(011)         VALUE
                                       '@@@REDECARD'.
           05  FILLER                  PIC  X(339)         VALUE SPACES.


       01  WRK-FS-ARQUIVOS.
           05  WRK-FS-AFILCOMP         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-CTRLAFIL         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-CTRLATUA         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-AFILIENV         PIC  X(002)         VALUE SPACES.
           05  WRK-FS-EATUFILI         PIC  X(002)         VALUE SPACES.

       01  WRK-PLAQUETA.
           05  WRK-PLAQUETA-1          PIC  X(011)         VALUE SPACES.
           05  WRK-PLAQUETA-2          PIC  X(011)         VALUE SPACES.

       01  WRK-DATE.
           05  WRK-DATE-DD             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DATE-MM             PIC  9(002)         VALUE ZEROS.
           05  FILLER                  PIC  X(001)         VALUE SPACES.
           05  WRK-DATE-AAAA           PIC  9(004)         VALUE ZEROS.

       01  WRK-DATE-RDF.
           05  WRK-DATE-RDF-AAAA       PIC  9(004)         VALUE ZEROS.
           05  WRK-DATE-RDF-MM         PIC  9(002)         VALUE ZEROS.
           05  WRK-DATE-RDF-DD         PIC  9(002)         VALUE ZEROS.
       01  FILLER REDEFINES WRK-DATE-RDF.
           05  WRK-DATA-PROPOSTA       PIC  9(008).

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

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA POOL7600 ***'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           05  WRK-DT-JULIANA          PIC  9(005)  COMP-3 VALUE ZEROS.
           05  WRK-DT-AAMMDD           PIC  9(007)  COMP-3 VALUE ZEROS.
           05  WRK-DT-AAAAMMDD         PIC  9(009)  COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSS           PIC  9(007)  COMP-3 VALUE ZEROS.
           05  WRK-TI-HHMMSSMMMMMM     PIC  9(013)  COMP-3 VALUE ZEROS.
           05  TIMESTAMP               PIC  X(020)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA POOL0431 ***'.
      *----------------------------------------------------------------*

       01  WRK-CAD-CONTA               PIC 9(013)          VALUE ZEROS.
       01  WRK-DIGITO                  PIC X(001)          VALUE SPACES.
       01  WRK-TAMANHO                 PIC 9(002)          VALUE 13.

      *---------------------------------------------------------------*
      *  OS CPOS ABAIXO DEVEM SER INICIALIZADOS SO' QDO O APLICATIVO  *
      *  USAR O SISTEMA DE SENHAS (BRAD1000).                         *
      *---------------------------------------------------------------*

           COPY 'I#BRAD7C'.

           05  WRK-ERR-DADOS-SENHAS.
               07  WRK-ERR-COD-USER    PIC  X(007)         VALUE SPACES.
               07  WRK-ERR-COD-DEPTO   PIC  X(006)         VALUE SPACES.
               07  WRK-ERR-MODULO      PIC  X(008)         VALUE SPACES.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*


      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIALIZACAO.

           WRITE BVVEGK-REG00-HEADER FROM WRK-REG-TRANSMISSAO.

           PERFORM 1130-TESTAR-FS-AFILIENV.

           PERFORM 2200-GRAVAR-HEADER.

           PERFORM 2000-PROCESSAR
             UNTIL WRK-FIM-AFILCOMP    EQUAL  'S'.

           PERFORM 2700-GRAVAR-CTRLATUA.

           PERFORM 2300-GRAVAR-TRAILER.

           PERFORM 3000-FINALIZACAO.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIALIZACAO              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  AFILCOMP
                       CTRLAFIL
                OUTPUT AFILIENV
                       CTRLATUA
                       EATUFILI.

           MOVE    WRK-ABERTURA        TO  WRK-OPERACAO
           PERFORM 1100-TESTAR-FILE-STATUS.
           PERFORM 1300-VERIFICA-VAZIO.

           MOVE BVVEGJ-ULT-NSU         TO WRK-NSU.
           MOVE BVVEGJ-ULTLOTE-ENVIADO TO WRK-NRO-LOTE.
           ADD 1                       TO WRK-NRO-LOTE.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS         SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-AFILCOMP.
           PERFORM 1120-TESTAR-FS-CTRLAFIL.
           PERFORM 1130-TESTAR-FS-AFILIENV.
           PERFORM 1140-TESTAR-FS-EATUFILI.
           PERFORM 1150-TESTAR-FS-CTRLATUA.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
       1110-TESTAR-FS-AFILCOMP         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AFILCOMP         NOT EQUAL '00'
               MOVE WRK-FS-AFILCOMP    TO  WRK-FILE-STATUS
               MOVE 'AFILCOMP'         TO  WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO  ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-CTRLAFIL         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLAFIL         NOT EQUAL '00'
               MOVE WRK-FS-CTRLAFIL    TO WRK-FILE-STATUS
               MOVE 'CTRLAFIL'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-AFILIENV         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-AFILIENV         NOT EQUAL '00'
               MOVE WRK-FS-AFILIENV    TO WRK-FILE-STATUS
               MOVE 'AFILIENV'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-EATUFILI         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-EATUFILI         NOT EQUAL '00'
               MOVE WRK-FS-EATUFILI    TO WRK-FILE-STATUS
               MOVE 'EATUFILI'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1150-TESTAR-FS-CTRLATUA         SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-CTRLATUA         NOT EQUAL '00'
               MOVE WRK-FS-CTRLATUA    TO WRK-FILE-STATUS
               MOVE 'CTRLATUA'         TO WRK-ARQUIVO
               MOVE WRK-ERRO-TEXTO     TO ERR-TEXTO
               PERFORM 9999-TRATAR-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1150-99-FIM. EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1300-VERIFICA-VAZIO             SECTION.
      *----------------------------------------------------------------*

           CALL  'POOL7600'  USING     WRK-DATA-HORA.

           PERFORM 1400-LER-AFILCOMP.

           IF  WRK-FIM-AFILCOMP        EQUAL 'S'
               DISPLAY '*********** BVVE6150 ************'
               DISPLAY '*                               *'
               DISPLAY '*  ARQUIVO AFILCOMP ESTA VAZIO  *'
               DISPLAY '*                               *'
               DISPLAY '*********** BVVE6150 ************'
               INITIALIZE              BVVEGJ-REG
               PERFORM 1500-LER-CTRLAFIL
               MOVE WRK-DT-AAAAMMDD    TO BVVEGJ-DTENVIO-ULTLOTE
               MOVE WRK-TI-HHMMSS      TO BVVEGJ-HRENVIO-ULTLOTE
               WRITE   REG-CTRLATUA FROM BVVEGJ-REG OF CTRLAFIL
               MOVE    WRK-GRAVACAO    TO WRK-OPERACAO
               PERFORM 1150-TESTAR-FS-CTRLATUA
               ADD     1               TO ACU-GRAVADOS-CTRLATUA
               PERFORM 3000-FINALIZACAO
           END-IF.

           PERFORM 1500-LER-CTRLAFIL.

           IF  WRK-FIM-CTRLAFIL        EQUAL 'S'
               DISPLAY '*********** BVVE6150 ************'
               DISPLAY '*                               *'
               DISPLAY '*  ARQUIVO CTRLAFIL ESTA VAZIO  *'
               DISPLAY '*                               *'
               DISPLAY '*********** BVVE6150 ************'
               INITIALIZE              BVVEGJ-REG
               MOVE ZEROS              TO BVVEGJ-ULT-NSU
                                          BVVEGJ-ULTLOTE-ENVIADO
           END-IF.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1400-LER-AFILCOMP               SECTION.
      *----------------------------------------------------------------*

           READ AFILCOMP
             AT END
                MOVE 'S'               TO WRK-FIM-AFILCOMP
            NOT AT END
                MOVE WRK-LEITURA       TO WRK-OPERACAO
                PERFORM 1110-TESTAR-FS-AFILCOMP
                ADD  1                 TO  ACU-LIDOS-R
           END-READ.

      *----------------------------------------------------------------*
       1400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1500-LER-CTRLAFIL               SECTION.
      *----------------------------------------------------------------*

           READ CTRLAFIL
             AT END
                MOVE 'S'               TO WRK-FIM-CTRLAFIL
            NOT AT END
                MOVE WRK-LEITURA       TO WRK-OPERACAO
                PERFORM 1120-TESTAR-FS-CTRLAFIL
                ADD 1                  TO ACU-LIDOS-A
           END-READ.

      *----------------------------------------------------------------*
       1500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO WRK-NSU.

           PERFORM 2100-MONTA-REGISTROS-GK.
           PERFORM 2600-GRAVAR-EATUFILI.
           PERFORM 1400-LER-AFILCOMP.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-MONTA-REGISTROS-GK         SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-IND-END
                                          WRK-IND-PROP.

           PERFORM 2400-GRAVAR-DADOS-BASICOS.
           PERFORM 2410-GRAVAR-DADOS-OPERACIONAIS.


           INITIALIZE BVVEGK-REG03-DADOSEND.

           MOVE 1                      TO WRK-IND-END
                                          BVVEGK03-TIPO-ENDERECO.
           PERFORM 2420-GRAVAR-DADOS-ENDERECOS.

           INITIALIZE BVVEGK-REG03-DADOSEND.

           MOVE 2                      TO WRK-IND-END
                                          BVVEGK03-TIPO-ENDERECO.

           IF  BVVEGI-ENDERECOS(WRK-IND-END)
                                       NOT EQUAL SPACES AND LOW-VALUES
               PERFORM 2420-GRAVAR-DADOS-ENDERECOS
           ELSE
               MOVE 1                  TO WRK-IND-END
               PERFORM 2420-GRAVAR-DADOS-ENDERECOS
           END-IF.

           PERFORM 2430-GRAVAR-DADOS-DOMIC-B.
           PERFORM 2450-GRAVAR-PRODUTOS.
           PERFORM 2460-GRAVAR-TECNOLOGIA-1.
           PERFORM 2470-GRAVAR-TECNOLOGIA-2.

           MOVE 2                      TO WRK-IND-PROP.
           MOVE 3                      TO WRK-IND-PROP-2.
           IF (BVVEGI-PROPRIETARIOS(WRK-IND-PROP)
                                       NOT EQUAL SPACES AND LOW-VALUES)
           AND(BVVEGI-PROPRIETARIOS(WRK-IND-PROP-2)
                                       NOT EQUAL SPACES AND LOW-VALUES)
               INITIALIZE BVVEGK-REG09-PROPRIET
               MOVE 1                  TO  WRK-IND-PROP
                                           BVVEGK09-SEQUENCIA
               MOVE  34,00             TO BVVEGK09-PARTICIP-ACIONARIA
               PERFORM 2440-GRAVAR-DADOS-PROP
               INITIALIZE BVVEGK-REG09-PROPRIET
               MOVE 2                  TO  WRK-IND-PROP
                                           BVVEGK09-SEQUENCIA
               MOVE  33,00             TO BVVEGK09-PARTICIP-ACIONARIA
               PERFORM 2440-GRAVAR-DADOS-PROP
               INITIALIZE BVVEGK-REG09-PROPRIET
               MOVE 3                  TO  WRK-IND-PROP
                                           BVVEGK09-SEQUENCIA
               MOVE  33,00             TO BVVEGK09-PARTICIP-ACIONARIA
               PERFORM 2440-GRAVAR-DADOS-PROP
           ELSE
               IF  BVVEGI-PROPRIETARIOS(WRK-IND-PROP)
                                       NOT EQUAL SPACES AND LOW-VALUES
                   INITIALIZE BVVEGK-REG09-PROPRIET
                   MOVE 1              TO  WRK-IND-PROP
                                           BVVEGK09-SEQUENCIA
                   MOVE  50,00         TO BVVEGK09-PARTICIP-ACIONARIA
                   PERFORM 2440-GRAVAR-DADOS-PROP
                   INITIALIZE BVVEGK-REG09-PROPRIET
                   MOVE 2              TO  WRK-IND-PROP
                                           BVVEGK09-SEQUENCIA
                   MOVE  50,00         TO BVVEGK09-PARTICIP-ACIONARIA
                   PERFORM 2440-GRAVAR-DADOS-PROP
               ELSE
                   INITIALIZE BVVEGK-REG09-PROPRIET
                   MOVE 1              TO  WRK-IND-PROP
                                           BVVEGK09-SEQUENCIA
                   MOVE 100,00         TO BVVEGK09-PARTICIP-ACIONARIA
                   PERFORM 2440-GRAVAR-DADOS-PROP
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2200-GRAVAR-HEADER              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEGK-REG00-HEADER.
           MOVE '1'                    TO BVVEGK00-TIPO-REG.
           MOVE ZEROS                  TO BVVEGK00-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK00-LITERAL.
           MOVE WRK-NRO-LOTE           TO BVVEGK00-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD          TO BVVEGK00-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK00-HORA-LOTE.
           MOVE '237'                  TO BVVEGK00-GERADOR-ARQ.
           MOVE 01                     TO BVVEGK00-GERACAO-ARQ.
           MOVE 'BANCO BRADESCO S.A.'  TO BVVEGK00-EMPRESA-GERADORA.
           MOVE ZEROS                  TO BVVEGK00-RESERVADO1.
           MOVE SPACES                 TO BVVEGK00-RESERVADO2.

           WRITE   BVVEGK-REG00-HEADER OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2300-GRAVAR-TRAILER             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEGK-REG99-TRAILER.
           MOVE '8'                    TO BVVEGK99-TIPO-REG.
           MOVE 00                     TO BVVEGK99-SUBTIPO-REG.
           MOVE 00                     TO BVVEGK99-LITERAL.
           MOVE WRK-NRO-LOTE           TO BVVEGK99-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK99-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK99-HORA-LOTE.
           MOVE '237'                  TO BVVEGK99-GERADOR-ARQ.
           MOVE ZEROS                  TO BVVEGK99-RESERVADO1.
           MOVE 01                     TO BVVEGK99-GERACAO-ARQ.
           MOVE ACU-GRAVADOS           TO BVVEGK99-QTDE-REGISTROS.
           MOVE SPACES                 TO BVVEGK99-RESERVADO2.
           WRITE   BVVEGK-REG99-TRAILER OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.


      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2400-GRAVAR-DADOS-BASICOS       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEGK-REG01-DADOSBAS.
           MOVE '5'                    TO BVVEGK01-TIPO-REG.
           MOVE 01                     TO BVVEGK01-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK01-LITERAL.
           MOVE WRK-NRO-LOTE           TO BVVEGK01-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK01-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK01-HORA-LOTE.
           MOVE '237'                  TO BVVEGK01-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK01-NSU.
           MOVE 01                     TO BVVEGK01-GERACAO-ARQ.
           MOVE 'E'                    TO BVVEGK01-TIPO-MOVTO.
           MOVE ZEROS                  TO BVVEGK01-INVESTIG-PROP.
           MOVE ZEROS                  TO BVVEGK01-NUMERO-PV.

7C2511*    IF  BVVEGI-CFLIAL-CNPJ-ESTBL
7C2511*                                GREATER ZEROS
7C2511     IF (BVVEGI-CFLIAL-CNPJ-ESTBL NOT EQUAL SPACES) AND
7C2511        (BVVEGI-CFLIAL-CNPJ-ESTBL NOT EQUAL LOW-VALUES)
               MOVE 'J'                TO BVVEGK01-TIPO-PESSOA
               MOVE BVVEGI-CCNPJ-CPF-ESTBL
                                       TO BVVEGK01-CNPJ-NRO
               MOVE BVVEGI-CFLIAL-CNPJ-ESTBL
                                       TO BVVEGK01-CNPJ-FILIAL
               MOVE BVVEGI-CCTRL-CNPJ-CPF
                                       TO BVVEGK01-CNPJ-CTRL
           ELSE
               MOVE 'F'                TO BVVEGK01-TIPO-PESSOA
               MOVE ZEROS              TO BVVEGK01-CPF-ZEROS
               MOVE BVVEGI-CCNPJ-CPF-ESTBL
                                       TO BVVEGK01-CPF-NRO
               MOVE BVVEGI-CCTRL-CNPJ-CPF
                                       TO BVVEGK01-CPF-CTRL
           END-IF.

           MOVE BVVEGI-CRAMO-ATVDD-ESTBL
                                       TO BVVEGK01-RAMO-ATIVIDADE.
           MOVE BVVEGI-RAZAO-SOCIAL    TO BVVEGK01-RAZAO-SOCIAL.
           MOVE BVVEGI-DATA-FUNDACAO   TO BVVEGK01-DATA-FUNDACAO.
           MOVE BVVEGI-ICNTAT-ESTBL-AFLIA
                                       TO BVVEGK01-NOME-CONTATO.
           IF  BVVEGI-EEMAIL-CNTAT-ESTBL
                                       NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'S'                TO BVVEGK01-ACESSO-INTERNET
           ELSE
               MOVE 'N'                TO BVVEGK01-ACESSO-INTERNET
           END-IF.
           MOVE BVVEGI-EEMAIL-CNTAT-ESTBL
                                       TO BVVEGK01-EMAIL-ESTAB.
           MOVE SPACES                 TO BVVEGK01-HOMEPAGE-ESTAB.
           MOVE BVVEGI-CDDD-FONE-ESTBL TO BVVEGK01-DDD1-TELEFONE.
           MOVE BVVEGI-CFONE-ESTBL-COML
                                       TO BVVEGK01-TELEFONE1.
           MOVE ZEROS                  TO BVVEGK01-RAMAL1-TELOFONE.
           MOVE SPACES                 TO BVVEGK01-DDD2-TELEFONE.
           MOVE ZEROS                  TO BVVEGK01-TELEFONE2.
           MOVE ZEROS                  TO BVVEGK01-RAMAL2-TELOFONE.
           MOVE BVVEGI-CDDD-FAX-ESTBL  TO BVVEGK01-DDD-FAX.
           MOVE BVVEGI-CFONE-FAX-ESTBL TO BVVEGK01-TELEFONE-FAX.
           MOVE SPACES                 TO BVVEGK01-CODIGO-CAMPANHA.
           IF  BVVEGI-CCRM-RESP-ESTBL  NOT EQUAL ZEROS
               MOVE BVVEGI-CCRM-RESP-ESTBL
                                       TO BVVEGK01-CODIDENT-CONSELHO
               MOVE 'CRM'              TO BVVEGK01-SIGLA-CONSELHO
           ELSE
               IF  BVVEGI-CCRO-RESP-ESTBL
                                       NOT EQUAL ZEROS
                   MOVE BVVEGI-CCRO-RESP-ESTBL
                                       TO BVVEGK01-CODIDENT-CONSELHO
                   MOVE 'CRO'          TO BVVEGK01-SIGLA-CONSELHO
               END-IF
           END-IF.

           MOVE    SPACES              TO BVVEGK01-RESERVADO.

           WRITE   BVVEGK-REG01-DADOSBAS OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2410-GRAVAR-DADOS-OPERACIONAIS  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE BVVEGK-REG02-DADOSOPE.
           MOVE '5'                    TO BVVEGK02-TIPO-REG.
           MOVE 02                     TO BVVEGK02-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK02-LITERAL.
           MOVE WRK-NRO-LOTE           TO BVVEGK02-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK02-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK02-HORA-LOTE.
           MOVE '237'                  TO BVVEGK02-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK02-NSU.
           MOVE 01                     TO BVVEGK02-GERACAO-ARQ.

           MOVE SPACES                 TO WRK-PLAQUETA.
           MOVE BVVEGI-IPLAQT-EQPMT-ESTBL
                                       TO WRK-PLAQUETA.
           IF  WRK-PLAQUETA-2          EQUAL SPACES OR LOW-VALUES
               MOVE BVVEGI-IPLAQT-EQPMT-ESTBL
                                       TO  BVVEGK02-PLAQUETA-PARTE2
           ELSE
               MOVE BVVEGI-IPLAQT-EQPMT-ESTBL
                                       TO BVVEGK02-NOME-PLAQUETA
           END-IF.

           MOVE ZEROS                  TO BVVEGK02-FILIAL-FILIACAO.
           MOVE 'V'                    TO BVVEGK02-GERENCIA-FILIACAO.
           MOVE ZEROS                  TO BVVEGK02-CARTEIRA.
           MOVE ZEROS                  TO BVVEGK02-ZONA-VENDA.

           EVALUATE BVVEGI-CTPO-FUNCN-ESTBL
               WHEN 'C'
                   MOVE 0              TO BVVEGK02-HORARIO-FUNCIONA
               WHEN 'N'
                   MOVE 1              TO BVVEGK02-HORARIO-FUNCIONA
           END-EVALUATE.

           MOVE 'N'                    TO BVVEGK02-ENVIO-MAQUINETA.
           MOVE ZEROS                  TO BVVEGK02-QTDE-MAQUINETA.
           MOVE 'N'                    TO BVVEGK02-CODIGO-IATA.

           EVALUATE  BVVEGI-CTPO-ESTBL-COML
               WHEN 'A'
                   MOVE 0              TO BVVEGK02-TIPO-ESTAB
               WHEN 'F'
                   MOVE 1              TO BVVEGK02-TIPO-ESTAB
               WHEN 'M'
                   MOVE 2              TO BVVEGK02-TIPO-ESTAB
           END-EVALUATE.

           MOVE 'S'                    TO BVVEGK02-TIPO-COMERCIALIZ.
           MOVE 'N'                    TO BVVEGK02-MARKET-CATALOGO.
           MOVE 'N'                    TO BVVEGK02-MARKET-TELEFONE.
           MOVE 'N'                    TO BVVEGK02-MARKET-ELETRONICO.
           MOVE BVVEGI-CPTO-VDA-MTRIZ  TO BVVEGK02-NUMERO-MATRIZ.
           MOVE BVVEGI-IESTBL-AFLIA    TO BVVEGK02-NOME-COMERCIAL.
           MOVE ZEROS                  TO BVVEGK02-CONSIGNACAO-PDV.
           MOVE ZEROS                  TO BVVEGK02-COD-CONSIGNADOR
                                          BVVEGK02-GRUPO-COMERCIAL
                                          BVVEGK02-GRUPO-GERENCIAL
                                          BVVEGK02-CENTRALIZ-MATER.
           EVALUATE BVVEGI-CLOC-PGTO-ESTBL
               WHEN 'E'
                   MOVE 1              TO BVVEGK02-LOCAL-PAGTO
               WHEN 'C'
                   MOVE 2              TO BVVEGK02-LOCAL-PAGTO
           END-EVALUATE.

           MOVE BVVEGI-CPTO-VDA-CTLZA  TO BVVEGK02-CENTRALIZ-PAGTO.
           MOVE 01                     TO BVVEGK02-CANAL-VENDA.
           MOVE 237                    TO BVVEGK02-CELULA-VENDA.
           MOVE BVVEGI-CAG-BCRIA       TO BVVEGK02-AGENCIA-FILIACAO.

7C2511*    IF  BVVEGI-CFLIAL-CNPJ-CONTT
7C2511*                                GREATER ZEROS
7C2511     IF (BVVEGI-CFLIAL-CNPJ-CONTT NOT EQUAL SPACES) AND
7C2511        (BVVEGI-CFLIAL-CNPJ-CONTT NOT EQUAL LOW-VALUES)
               MOVE BVVEGI-CCNPJ-CPF-CONTT
                                       TO BVVEGK02-CNPJ-NRO
               MOVE BVVEGI-CFLIAL-CNPJ-CONTT
                                       TO BVVEGK02-CNPJ-FILIAL
               MOVE BVVEGI-CCTRL-CNPJ-CONTT
                                       TO BVVEGK02-CNPJ-CTRL

           ELSE
               MOVE ZEROS              TO BVVEGK02-CPF-ZEROS
               MOVE BVVEGI-CCNPJ-CPF-CONTT
                                       TO BVVEGK02-CPF-NRO
               MOVE BVVEGI-CCTRL-CNPJ-CONTT
                                       TO BVVEGK02-CPF-CTRL
           END-IF.

           MOVE ZEROS                  TO BVVEGK02-INSTALADOR-EQPT.
           MOVE BVVEGI-DINCL-ESTBL-COML
                                       TO WRK-DATE.
           MOVE WRK-DATE-AAAA          TO WRK-DATE-RDF-AAAA.
           MOVE WRK-DATE-MM            TO WRK-DATE-RDF-MM.
           MOVE WRK-DATE-DD            TO WRK-DATE-RDF-DD.
           MOVE WRK-DATA-PROPOSTA      TO BVVEGK02-DATA-PROPOSTA.
           MOVE 'S'                    TO BVVEGK02-FLAG-TECNOLOGIA.
           MOVE 'C'                    TO BVVEGK02-LOCALIZACAO-FIS.
           MOVE 'B'                    TO BVVEGK02-CONSERVACAO-LOJA.
           MOVE BVVEGI-CLOC-ESTBL-COML TO BVVEGK02-REGIAO-LOJA.
           MOVE ZEROS                  TO BVVEGK02-TAXA-ADESAO.
           MOVE SPACES                 TO BVVEGK02-TRABCOM-VISA
                                          BVVEGK02-TRABCOM-AMEX
                                          BVVEGK02-TRABCOM-OUTROS.
           MOVE BVVEGI-CENDER-COML-IGUAL
                                       TO BVVEGK02-ENDERECO-IGUAL.

           MOVE SPACES                 TO BVVEGK02-RESERVADO.

           WRITE   BVVEGK-REG02-DADOSOPE OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2420-GRAVAR-DADOS-ENDERECOS     SECTION.
      *----------------------------------------------------------------*

           MOVE '5'                    TO BVVEGK03-TIPO-REG.
           MOVE 03                     TO BVVEGK03-SUBTIPO-REG.
           MOVE WRK-NRO-LOTE           TO BVVEGK03-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK03-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK03-HORA-LOTE.
           MOVE '237'                  TO BVVEGK03-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK03-NSU.
           MOVE 01                     TO BVVEGK03-GERACAO-ARQ.
           MOVE BVVEGI-ELOGDR-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK03-ENDERECO-ESTAB.
           MOVE BVVEGI-ECOMPL-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK03-COMPLEMENTO-END.
           MOVE BVVEGI-ELOGDR-NRO-ESTBL(WRK-IND-END)
                                       TO BVVEGK03-NUMERO-END.
           MOVE BVVEGI-EBAIRO-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK03-BAIRRO.
           MOVE BVVEGI-ECIDDE-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK03-CIDADE.
           MOVE BVVEGI-CSGL-UF(WRK-IND-END)
                                       TO BVVEGK03-ESTADO.
           MOVE BVVEGI-CCEP(WRK-IND-END)
                                       TO BVVEGK03-CEP.
           MOVE BVVEGI-CCEP-COMPL(WRK-IND-END)
                                       TO BVVEGK03-CEP-COMPLEMENTO.

           MOVE SPACES                 TO BVVEGK03-RESERVADO.

           WRITE   BVVEGK-REG03-DADOSEND OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2420-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2430-GRAVAR-DADOS-DOMIC-B       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  BVVEGK-REG04-DOMICILIOB.
           MOVE '5'                    TO BVVEGK04-TIPO-REG.
           MOVE 04                     TO BVVEGK04-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK04-LITERAL.
           MOVE WRK-NRO-LOTE           TO BVVEGK04-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK04-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK04-HORA-LOTE.
           MOVE '237'                  TO BVVEGK04-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK04-NSU.
           MOVE 01                     TO BVVEGK04-GERACAO-ARQ.
           MOVE 01                     TO BVVEGK04-CREDITO-OPER.
           MOVE 237                    TO BVVEGK04-CREDITO-BANCO.
           MOVE BVVEGI-CAG-BCRIA       TO BVVEGK04-CREDITO-AGENCIA.
           MOVE BVVEGI-CCTA-BCRIA-CLI  TO BVVEGK04-CREDITO-CTANRO
                                          WRK-CAD-CONTA.
           MOVE SPACES                 TO WRK-DIGITO.
           CALL  'POOL0431'            USING  WRK-CAD-CONTA
                                              WRK-DIGITO
                                              WRK-TAMANHO.
           IF WRK-DIGITO EQUAL '.'
              DISPLAY '************ BVVE6150 ************'
              DISPLAY '*                                *'
              DISPLAY '*        ERRO NA POOL0431        *'
              DISPLAY '*       PROGRAMA CANCELADO       *'
              DISPLAY '*                                *'
              DISPLAY '**********************************'
              MOVE '** ERRO NA POOL0431 **'  TO  ERR-TEXTO
              PERFORM 9999-TRATAR-ERRO
           END-IF.

           IF WRK-DIGITO EQUAL 'P'
              MOVE '0' TO WRK-DIGITO
           END-IF.

           MOVE WRK-DIGITO             TO BVVEGK04-CREDITO-CTADIG.
           MOVE 02                     TO BVVEGK04-MAESTRO-OPER.
           MOVE 237                    TO BVVEGK04-MAESTRO-BANCO.
           MOVE BVVEGI-CAG-BCRIA       TO BVVEGK04-MAESTRO-AGENCIA.
           MOVE BVVEGI-CCTA-BCRIA-CLI  TO BVVEGK04-MAESTRO-CTANRO.
           MOVE WRK-DIGITO             TO BVVEGK04-MAESTRO-CTADIG.
           MOVE 03                     TO BVVEGK04-REDECARD-OPER.
           MOVE 237                    TO BVVEGK04-REDECARD-BANCO.
           MOVE BVVEGI-CAG-BCRIA       TO BVVEGK04-REDECARD-AGENCIA.
           MOVE BVVEGI-CCTA-BCRIA-CLI  TO BVVEGK04-REDECARD-CTANRO.
           MOVE WRK-DIGITO             TO BVVEGK04-REDECARD-CTADIG.
           MOVE ZEROS                  TO BVVEGK04-CONSTRU-OPER
                                       BVVEGK04-CONSTRU-BANCO
                                       BVVEGK04-CONSTRU-AGENCIA
                                       BVVEGK04-CONSTRU-CTANRO
                                       BVVEGK04-CONSTRU-CTADIG.

           MOVE SPACES                 TO BVVEGK04-RESERVADO.

           WRITE   BVVEGK-REG04-DOMICILIOB OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2430-99-FIM.                     EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2440-GRAVAR-DADOS-PROP          SECTION.
      *----------------------------------------------------------------*

           MOVE '5'                    TO BVVEGK09-TIPO-REG.
           MOVE 09                     TO BVVEGK09-SUBTIPO-REG.
           MOVE WRK-NRO-LOTE           TO BVVEGK09-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK09-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK09-HORA-LOTE.
           MOVE '237'                  TO BVVEGK09-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK09-NSU.
           MOVE 01                     TO BVVEGK09-GERACAO-ARQ.
           MOVE BVVEGI-IPPRIE-ESTBL-AFLIA(WRK-IND-PROP)
                                       TO BVVEGK09-NOME-PROPRIET.

           MOVE BVVEGI-DNASC-PPRIE (WRK-IND-PROP)
                                       TO BVVEGK09-DATA-NASCIMENTO.

           MOVE ZEROS                  TO BVVEGK09-CPF-ZEROS.

           MOVE BVVEGI-CCPF-PPRIE(WRK-IND-PROP)
                                       TO BVVEGK09-CPF-NRO.

           MOVE BVVEGI-CCTRL-CPF-PPRIE(WRK-IND-PROP)
                                       TO BVVEGK09-CPF-CTRL.

           MOVE 'F'                    TO BVVEGK09-TIPO-PESSOA.

           MOVE SPACES                 TO BVVEGK09-RESERVADO.

           WRITE   BVVEGK-REG09-PROPRIET OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2440-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2450-GRAVAR-PRODUTOS            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVEGK-REG05-PRODUTOS.
           MOVE '5'                    TO BVVEGK05-TIPO-REG.
           MOVE 05                     TO BVVEGK05-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK05-LITERAL.
           MOVE WRK-NRO-LOTE           TO BVVEGK05-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK05-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK05-HORA-LOTE.
           MOVE '237'                  TO BVVEGK05-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK05-NSU.
           MOVE 01                     TO BVVEGK05-GERACAO-ARQ.

           MOVE 01                     TO BVVEGK05-MASTERL1-CCA.
           MOVE 'C'                    TO BVVEGK05-MASTERL1-DEBCRE.
           MOVE 01                     TO BVVEGK05-MASTERL1-FEAT.
           MOVE 01                     TO BVVEGK05-MASTERL1-OPER.
           MOVE 031                    TO BVVEGK05-MASTERL1-PRAZO.
           MOVE BVVEGI-PTX-CREDT-ROTVO TO BVVEGK05-MASTERL1-TAXA.
           MOVE ZEROS                  TO BVVEGK05-MASTERL1-LPARCELA.

           MOVE 02                     TO BVVEGK05-MASTERI-CCA.
           MOVE 'C'                    TO BVVEGK05-MASTERI-DEBCRE.
           MOVE 01                     TO BVVEGK05-MASTERI-FEAT.
           MOVE 01                     TO BVVEGK05-MASTERI-OPER.
           MOVE 031                    TO BVVEGK05-MASTERI-PRAZO.
           MOVE BVVEGI-PTX-VDA-INTL    TO BVVEGK05-MASTERI-TAXA.
           MOVE ZEROS                  TO BVVEGK05-MASTERI-RESERVADO.

           MOVE 03                     TO BVVEGK05-DINERSL1-CCA.
           MOVE 'C'                    TO BVVEGK05-DINERSL1-DEBCRE.
           MOVE 01                     TO BVVEGK05-DINERSL1-FEAT.
           MOVE 01                     TO BVVEGK05-DINERSL1-OPER.
           MOVE 031                    TO BVVEGK05-DINERSL1-PRAZO.
           MOVE BVVEGI-PTX-CREDT-ROTVO TO BVVEGK05-DINERSL1-TAXA.
           MOVE ZEROS                  TO BVVEGK05-DINERSL1-LPARCELA.

           MOVE 04                     TO BVVEGK05-DINERSI-CCA.
           MOVE 'C'                    TO BVVEGK05-DINERSI-DEBCRE.
           MOVE 01                     TO BVVEGK05-DINERSI-FEAT.
           MOVE 01                     TO BVVEGK05-DINERSI-OPER.
           MOVE 031                    TO BVVEGK05-DINERSI-PRAZO.
           MOVE BVVEGI-PTX-VDA-INTL    TO BVVEGK05-DINERSI-TAXA.
           MOVE ZEROS                  TO BVVEGK05-DINERSI-RESERVADO.

           MOVE 05                     TO BVVEGK05-REDEDEB4-CCA.
           MOVE 'D'                    TO BVVEGK05-REDEDEB4-DEBCRE.
           MOVE 04                     TO BVVEGK05-REDEDEB4-FEAT.
           MOVE 03                     TO BVVEGK05-REDEDEB4-OPER.
           MOVE 001                    TO BVVEGK05-REDEDEB4-PRAZO.
           MOVE BVVEGI-PTX-CATAO-DEB   TO BVVEGK05-REDEDEB4-TAXA.
           MOVE ZEROS                  TO BVVEGK05-REDEDEB4-RESERVADO.

           MOVE 20                     TO BVVEGK05-REDECRE1-CCA.
           MOVE 'C'                    TO BVVEGK05-REDECRE1-DEBCRE.
           MOVE 01                     TO BVVEGK05-REDECRE1-FEAT.
           MOVE 01                     TO BVVEGK05-REDECRE1-OPER.
           MOVE 031                    TO BVVEGK05-REDECRE1-PRAZO.
           MOVE BVVEGI-PTX-CREDT-ROTVO TO BVVEGK05-REDECRE1-TAXA.
           MOVE ZEROS                  TO BVVEGK05-REDECRE1-LPARCELA.

           MOVE 14                     TO BVVEGK05-MAESTROCDC-CCA.
           MOVE 'D'                    TO BVVEGK05-MAESTROCDC-DEBCRE.
           MOVE 16                     TO BVVEGK05-MAESTROCDC-FEAT.
           MOVE 02                     TO BVVEGK05-MAESTROCDC-OPER.
           MOVE 001                    TO BVVEGK05-MAESTROCDC-PRAZO.
           MOVE BVVEGI-PTX-CATAO-DEB   TO BVVEGK05-MAESTROCDC-TAXA.
           MOVE ZEROS                  TO BVVEGK05-MAESTROCDC-RESERVADO.

           MOVE 14                     TO BVVEGK05-MAESTRODEB-CCA.
           MOVE 'D'                    TO BVVEGK05-MAESTRODEB-DEBCRE.
           MOVE 04                     TO BVVEGK05-MAESTRODEB-FEAT.
           MOVE 02                     TO BVVEGK05-MAESTRODEB-OPER.
           MOVE 001                    TO BVVEGK05-MAESTRODEB-PRAZO.
           MOVE BVVEGI-PTX-CATAO-DEB   TO BVVEGK05-MAESTRODEB-TAXA.
           MOVE ZEROS                  TO BVVEGK05-MAESTRODEB-RESERVADO.

           MOVE ZEROS                  TO BVVEGK05-CONSTRU-CCA.
           MOVE SPACES                 TO BVVEGK05-CONSTRU-DEBCRE.
           MOVE ZEROS                  TO BVVEGK05-CONSTRU-FEAT.
           MOVE ZEROS                  TO BVVEGK05-CONSTRU-OPER.
           MOVE ZEROS                  TO BVVEGK05-CONSTRU-PRAZO.
           MOVE ZEROS                  TO BVVEGK05-CONSTRU-TAXA.
           MOVE ZEROS                  TO BVVEGK05-CONSTRU-RESERVADO.

           MOVE 01                     TO BVVEGK05-MASTERL2-CCA.
           MOVE 'C'                    TO BVVEGK05-MASTERL2-DEBCRE.
           MOVE 02                     TO BVVEGK05-MASTERL2-FEAT.
           MOVE 01                     TO BVVEGK05-MASTERL2-OPER.
           MOVE 31                     TO BVVEGK05-MASTERL2-PRAZO.
           MOVE BVVEGI-PTX-PCELD-JURO  TO BVVEGK05-MASTERL2-TAXA.
           MOVE ZEROS                  TO BVVEGK05-MASTERL2-LPARCELA.

           MOVE 03                     TO BVVEGK05-DINERSL2-CCA.
           MOVE 'C'                    TO BVVEGK05-DINERSL2-DEBCRE.
           MOVE 02                     TO BVVEGK05-DINERSL2-FEAT.
           MOVE 01                     TO BVVEGK05-DINERSL2-OPER.
           MOVE 031                    TO BVVEGK05-DINERSL2-PRAZO.
           MOVE BVVEGI-PTX-PCELD-JURO  TO BVVEGK05-DINERSL2-TAXA.
           MOVE ZEROS                  TO BVVEGK05-DINERSL2-LPARCELA.

           MOVE 05                     TO BVVEGK05-REDEDEB5-CCA.
           MOVE 'D'                    TO BVVEGK05-REDEDEB5-DEBCRE.
           MOVE 05                     TO BVVEGK05-REDEDEB5-FEAT.
           MOVE 03                     TO BVVEGK05-REDEDEB5-OPER.
           MOVE 001                    TO BVVEGK05-REDEDEB5-PRAZO.
           MOVE BVVEGI-PTX-CATAO-DEB   TO BVVEGK05-REDEDEB5-TAXA.
           MOVE ZEROS                  TO BVVEGK05-REDEDEB5-RESERVADO.

           MOVE 20                     TO BVVEGK05-REDECRE2-CCA.
           MOVE 'C'                    TO BVVEGK05-REDECRE2-DEBCRE.
           MOVE 02                     TO BVVEGK05-REDECRE2-FEAT.
           MOVE 01                     TO BVVEGK05-REDECRE2-OPER.
           MOVE 031                    TO BVVEGK05-REDECRE2-PRAZO.
           MOVE BVVEGI-PTX-PCELD-JURO  TO BVVEGK05-REDECRE2-TAXA.
           MOVE ZEROS                  TO BVVEGK05-REDECRE2-LPARCELA.

           MOVE 01                     TO BVVEGK05-MASTERL3-CCA.
           MOVE 'C'                    TO BVVEGK05-MASTERL3-DEBCRE.
           MOVE 03                     TO BVVEGK05-MASTERL3-FEAT.
           MOVE 01                     TO BVVEGK05-MASTERL3-OPER.
           MOVE 031                    TO BVVEGK05-MASTERL3-PRAZO.
           MOVE BVVEGI-PTX-CREDT-PCELD TO BVVEGK05-MASTERL3-TAXA.
           MOVE ZEROS                  TO BVVEGK05-MASTERL3-LPARCELA.

           MOVE 03                     TO BVVEGK05-DINERSL3-CCA.
           MOVE 'C'                    TO BVVEGK05-DINERSL3-DEBCRE.
           MOVE 03                     TO BVVEGK05-DINERSL3-FEAT.
           MOVE 01                     TO BVVEGK05-DINERSL3-OPER.
           MOVE 031                    TO BVVEGK05-DINERSL3-PRAZO.
           MOVE BVVEGI-PTX-CREDT-PCELD TO BVVEGK05-DINERSL3-TAXA.
           MOVE ZEROS                  TO BVVEGK05-DINERSL3-LPARCELA.

           MOVE 20                     TO BVVEGK05-REDECRE3-CCA.
           MOVE 'C'                    TO BVVEGK05-REDECRE3-DEBCRE.
           MOVE 03                     TO BVVEGK05-REDECRE3-FEAT.
           MOVE 01                     TO BVVEGK05-REDECRE3-OPER.
           MOVE 031                    TO BVVEGK05-REDECRE3-PRAZO.
           MOVE BVVEGI-PTX-CREDT-PCELD TO BVVEGK05-REDECRE3-TAXA.
           MOVE ZEROS                  TO BVVEGK05-REDECRE3-LPARCELA.

           WRITE   BVVEGK-REG05-PRODUTOS OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2450-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2460-GRAVAR-TECNOLOGIA-1        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVEGK-REG07-DADOSTEC.
           MOVE '5'                    TO BVVEGK07-TIPO-REG.
           MOVE 07                     TO BVVEGK07-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK07-SEQ.
           MOVE WRK-NRO-LOTE           TO BVVEGK07-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK07-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK07-HORA-LOTE.
           MOVE '237'                  TO BVVEGK07-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK07-NSU.
           MOVE 01                     TO BVVEGK07-GERACAO-ARQ.
           MOVE 'POS'                  TO BVVEGK07-TIPO-EQUIP.
           MOVE 001                    TO BVVEGK07-QTDE-EQUIP.
           MOVE 1                      TO BVVEGK07-PROPR-EQUIP.
           MOVE 2                      TO BVVEGK07-TIPO-LINHA.
           MOVE 'N'                    TO BVVEGK07-FLAG-HABILIT.
           MOVE 1                      TO WRK-IND-END.
           MOVE BVVEGI-ELOGDR-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK07-ENDERECO-INSTAL.
           MOVE BVVEGI-ECOMPL-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK07-COMPLEMENTO-INSTAL.
           MOVE BVVEGI-ELOGDR-NRO-ESTBL(WRK-IND-END)
                                       TO BVVEGK07-NUMERO-END-INSTAL.
           MOVE BVVEGI-EBAIRO-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK07-BAIRRO-INSTAL.
           MOVE BVVEGI-ECIDDE-ESTBL-COML(WRK-IND-END)
                                       TO BVVEGK07-CIDADE-INSTAL.
           MOVE BVVEGI-CSGL-UF(WRK-IND-END)
                                       TO BVVEGK07-ESTADO-INSTAL.
           MOVE BVVEGI-CCEP(WRK-IND-END)
                                       TO BVVEGK07-CEP-INSTAL.
           MOVE BVVEGI-CCEP-COMPL(WRK-IND-END)
                                       TO BVVEGK07-COMPL-CEP-INSTAL.
           MOVE BVVEGI-ICNTAT-ESTBL-AFLIA
                                       TO BVVEGK07-CONTATO-INSTAL.
           MOVE SPACES                 TO BVVEGK07-DDD-INSTAL.
           MOVE ZEROS                  TO BVVEGK07-TELEFONE-INSTAL.
           MOVE ZEROS                  TO BVVEGK07-RAMAL-INSTAL.
           MOVE SPACES                 TO BVVEGK07-FAB-HARD.
           MOVE SPACES                 TO BVVEGK07-FORN-SOFT.
           MOVE ZEROS                  TO BVVEGK07-NRO-RENPAC.
           MOVE ZEROS                  TO BVVEGK07-DIA-INIC-FUNC.
           MOVE ZEROS                  TO BVVEGK07-DIA-FIM-FUNC.
           MOVE ZEROS                  TO BVVEGK07-HH-INIC-FUNC-SEM.
           MOVE 1800                   TO BVVEGK07-HH-FIM-FUNC-SEM.
           MOVE ZEROS                  TO BVVEGK07-COD-REG-TECNO.
           MOVE ZEROS                  TO BVVEGK07-COD-CENTRO-CUSTO.
           MOVE 80,00                  TO BVVEGK07-PRECO-VIG-EQUIP-DTA.
           MOVE ZEROS                  TO BVVEGK07-COD-FILIAL-TECNO.
           MOVE 'S'                    TO BVVEGK07-END-INSTAL-ICOM.
           MOVE ZEROS                  TO BVVEGK07-COD-CENARIO.
           MOVE ZEROS                  TO BVVEGK07-COD-REDE.
           MOVE ZEROS                  TO BVVEGK07-COD-TIPO-CONEXAO.
           MOVE SPACES                 TO BVVEGK07-RESERVADO.

           WRITE   BVVEGK-REG07-DADOSTEC OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2460-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2470-GRAVAR-TECNOLOGIA-2        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  BVVEGK-REG08-DADOSTEC.
           MOVE '5'                    TO BVVEGK08-TIPO-REG.
           MOVE 08                     TO BVVEGK08-SUBTIPO-REG.
           MOVE 01                     TO BVVEGK08-SEQ.
           MOVE WRK-NRO-LOTE           TO BVVEGK08-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGK08-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGK08-HORA-LOTE.
           MOVE '237'                  TO BVVEGK08-GERADOR-ARQ.
           MOVE WRK-NSU                TO BVVEGK08-NSU.
           MOVE 01                     TO BVVEGK08-GERACAO-ARQ.
           MOVE SPACES                 TO BVVEGK08-OBSERVACOES.
           MOVE SPACES                 TO BVVEGK08-RESERVADO.

           WRITE   BVVEGK-REG08-DADOSTEC OF AFILIENV.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1130-TESTAR-FS-AFILIENV.

           ADD     1                   TO ACU-GRAVADOS.

      *----------------------------------------------------------------*
       2470-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2600-GRAVAR-EATUFILI            SECTION.
      *----------------------------------------------------------------*

           MOVE BVVEGI-CCNPJ-CPF-ESTBL TO BVVEGL-CCNPJ-CPF-ESTBL.
           MOVE BVVEGI-CFLIAL-CNPJ-ESTBL
                                       TO BVVEGL-CFLIAL-CNPJ-ESTBL.
           MOVE BVVEGI-CCTRL-CNPJ-CPF  TO BVVEGL-CCTRL-CNPJ-CPF.
           MOVE BVVEGI-IESTBL-AFLIA    TO BVVEGL-IESTBL-AFLIA.
           MOVE BVVEGI-IPLAQT-EQPMT-ESTBL
                                       TO BVVEGL-IPLAQT-EQPMT-ESTBL.
           MOVE BVVEGI-CBCO            TO BVVEGL-CBCO.
           MOVE BVVEGI-CAG-BCRIA       TO BVVEGL-CAG-BCRIA.
           MOVE BVVEGI-CCTA-BCRIA-CLI  TO BVVEGL-CCTA-BCRIA-CLI.
           MOVE BVVEGI-CSIT-PPSTA-AFLIA
                                       TO BVVEGL-CSIT-PPSTA-AFLIA.
           MOVE BVVEGI-CCNPJ-CPF-CONTT TO BVVEGL-CCNPJ-CPF-CONTT.
           MOVE BVVEGI-CFLIAL-CNPJ-CONTT
                                       TO BVVEGL-CFLIAL-CNPJ-CONTT.
           MOVE BVVEGI-CCTRL-CNPJ-CONTT
                                       TO BVVEGL-CCTRL-CNPJ-CONTT.
           MOVE BVVEGI-CRAMO-ATVDD-ESTBL
                                       TO BVVEGL-CRAMO-ATVDD-ESTBL.
           MOVE BVVEGI-CCRM-RESP-ESTBL TO BVVEGL-CCRM-RESP-ESTBL.
           MOVE BVVEGI-CCRO-RESP-ESTBL TO BVVEGL-CCRO-RESP-ESTBL
           MOVE BVVEGI-ICNTAT-ESTBL-AFLIA
                                       TO BVVEGL-ICNTAT-ESTBL-AFLIA.
           MOVE BVVEGI-CENDER-COML-IGUAL
                                       TO BVVEGL-CENDER-COML-IGUAL.
           MOVE BVVEGI-CDDD-FONE-ESTBL TO BVVEGL-CDDD-FONE-ESTBL.
           MOVE BVVEGI-CFONE-ESTBL-COML
                                       TO BVVEGL-CFONE-ESTBL-COML.
           MOVE BVVEGI-CDDD-FAX-ESTBL  TO BVVEGL-CDDD-FAX-ESTBL.
           MOVE BVVEGI-CFONE-FAX-ESTBL TO BVVEGL-CFONE-FAX-ESTBL.
           MOVE BVVEGI-EEMAIL-CNTAT-ESTBL
                                       TO BVVEGL-EEMAIL-CNTAT-ESTBL.
           MOVE BVVEGI-CTPO-FUNCN-ESTBL
                                       TO BVVEGL-CTPO-FUNCN-ESTBL.
           MOVE BVVEGI-CTPO-ESTBL-COML TO BVVEGL-CTPO-ESTBL-COML.
           MOVE BVVEGI-CPTO-VDA-MTRIZ  TO BVVEGL-CPTO-VDA-MTRIZ.
           MOVE BVVEGI-CPTO-VDA-CTLZA  TO BVVEGL-CPTO-VDA-CTLZA.
           MOVE BVVEGI-CLOC-PGTO-ESTBL TO BVVEGL-CLOC-PGTO-ESTBL.
           MOVE BVVEGI-CLOC-ESTBL-COML TO BVVEGL-CLOC-ESTBL-COML.
           MOVE BVVEGI-DINCL-ESTBL-COML
                                       TO BVVEGL-DINCL-ESTBL-COML.
           MOVE BVVEGI-CEMPR-INC       TO BVVEGL-CEMPR-INC.
           MOVE BVVEGI-CDEPDC          TO BVVEGL-CDEPDC.
           MOVE BVVEGI-CFUNC-BDSCO     TO BVVEGL-CFUNC-BDSCO.
           MOVE BVVEGI-CFUNC-ALT-ESTBL TO BVVEGL-CFUNC-ALT-ESTBL.
           MOVE BVVEGI-DALT-ESTBL-COML TO BVVEGL-DALT-ESTBL-COML.
           MOVE BVVEGI-PTX-PCELD-JURO  TO BVVEGL-PTX-PCELD-JURO.
           MOVE BVVEGI-PTX-CREDT-ROTVO TO BVVEGL-PTX-CREDT-ROTVO.
           MOVE BVVEGI-PTX-CREDT-PCELD TO BVVEGL-PTX-CREDT-PCELD.
           MOVE BVVEGI-PTX-VDA-INTL    TO BVVEGL-PTX-VDA-INTL.
           MOVE BVVEGI-PTX-CATAO-DEB   TO BVVEGL-PTX-CATAO-DEB.
           MOVE BVVEGI-RAZAO-SOCIAL    TO BVVEGL-RAZAO-SOCIAL.
           MOVE BVVEGI-DATA-FUNDACAO   TO BVVEGL-DATA-FUNDACAO.
           MOVE BVVEGI-ENDERECOS(1)    TO BVVEGL-ENDERECOS(1).
           IF  BVVEGI-ENDERECOS(2)     NOT EQUAL SPACES AND LOW-VALUES
               MOVE BVVEGI-ENDERECOS(2)
                                       TO BVVEGL-ENDERECOS(2)
           END-IF.
           MOVE BVVEGI-PROPRIETARIOS(1)
                                       TO BVVEGL-PROPRIETARIOS(1).
           IF  BVVEGI-PROPRIETARIOS(2) NOT EQUAL SPACES AND LOW-VALUES
               MOVE BVVEGI-PROPRIETARIOS(2)
                                       TO BVVEGL-PROPRIETARIOS(2)
           END-IF.
           IF  BVVEGI-PROPRIETARIOS(3) NOT EQUAL SPACES AND LOW-VALUES
               MOVE BVVEGI-PROPRIETARIOS(3)
                                       TO BVVEGL-PROPRIETARIOS(3)
           END-IF.

           MOVE WRK-NRO-LOTE           TO BVVEGL-NRO-LOTE.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGL-DATA-LOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGL-HORA-LOTE.
           MOVE WRK-NSU                TO BVVEGL-NSU.
           WRITE   BVVEGL-REG          OF EATUFILI.
           ADD     1                   TO ACU-GRAVADOS-EATUFILI
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1140-TESTAR-FS-EATUFILI.

      *----------------------------------------------------------------*
       2600-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2700-GRAVAR-CTRLATUA            SECTION.
      *----------------------------------------------------------------*

           ADD  1                      TO ACU-GRAVADOS-CTRLATUA.
           MOVE WRK-NRO-LOTE           TO BVVEGJ-ULTLOTE-ENVIADO.
           MOVE WRK-DT-AAAAMMDD        TO BVVEGJ-DTENVIO-ULTLOTE.
           MOVE WRK-TI-HHMMSS          TO BVVEGJ-HRENVIO-ULTLOTE.
           MOVE WRK-NSU                TO BVVEGJ-ULT-NSU.
           MOVE ACU-GRAVADOS-CTRLATUA  TO BVVEGJ-QTDEREG-LOTE.

           WRITE   REG-CTRLATUA FROM   BVVEGJ-REG OF CTRLAFIL.
           MOVE    WRK-GRAVACAO        TO WRK-OPERACAO
           PERFORM 1150-TESTAR-FS-CTRLATUA.

      *----------------------------------------------------------------*
       2700-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZACAO                SECTION.
      *----------------------------------------------------------------*

           CLOSE AFILCOMP
                 CTRLAFIL
                 CTRLATUA
                 AFILIENV
                 EATUFILI.

           MOVE    WRK-FECHAMENTO           TO  WRK-OPERACAO.
           PERFORM 1100-TESTAR-FILE-STATUS.

           DISPLAY '***************** B V E E 6 1 5 0 ****************'.
           DISPLAY '*         RESUMO DO PROCESSAMENTO                *'.
           DISPLAY '* TOTAL DE REGS.LIDOS AFILIACOES PENDENTES:      *'.
           DISPLAY '*              ' ACU-LIDOS-R '                   *'.
           DISPLAY '* TOTAL DE REGS.LIDOS CONTROLE AFILIACOES :      *'.
           DISPLAY '*              ' ACU-LIDOS-A '                   *'.
           DISPLAY '* TOTAL DE REGS.GRAV. CONTROLE AFILIACOES       :*'.
           DISPLAY '*              ' ACU-GRAVADOS-CTRLATUA '         *'.
           DISPLAY '* TOTAL DE REGS.GRAV. AFILEMV                   :*'.
           DISPLAY '*              ' ACU-GRAVADOS '                  *'.
           DISPLAY '* TOTAL DE REGS.GRAV. EATUFILI                  :*'.
           DISPLAY '*              ' ACU-GRAVADOS-EATUFILI '         *'.
           DISPLAY '***************** B V E E 6 1 5 0 ****************'.

           STOP RUN.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-TRATAR-ERRO            SECTION.
      *----------------------------------------------------------------*

           MOVE 'BVVE6150'           TO ERR-PGM
           MOVE 'APL'                TO ERR-TIPO-ACESSO
           MOVE SPACES               TO WRK-ERR-DADOS-SENHAS

           CALL 'BRAD7100' USING WRK-BATCH
                                 ERRO-AREA.
           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
