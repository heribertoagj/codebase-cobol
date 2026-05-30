      *===============================================================*
       IDENTIFICATION DIVISION.
      *===============================================================*
       PROGRAM-ID.    GFCT2825.
      *===============================================================*
      *                            G P T I                            *
      *---------------------------------------------------------------*
      *    PROGRAMA....:  GFCT2825                                    *
      *    ANALISTA....:  J.C.R.                                      *
      *    DATA........:  26/11/2008                                  *
      *---------------------------------------------------------------*
      *    OBJETIVO....:  GERAR ARQUIVO DE IDENTIFICACAO DOS CLIENTES *
      *                   PARA PUBLICO DE VALIDACAO (ASSUNTO 1000)    *
      *                   E GERA ARQUIVO DE REJEITADOS NO CLIE        *
      *---------------------------------------------------------------*
      *    ARQUIVOS....:                                              *
      *         DDNAME                           INCLUDE/BOOK         *
      *         BASECONT                           --------           *
      *         MESTRCLI                           --------           *
      *         REJECLIE                           GFCTWAD3           *
      *===============================================================*

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

           SELECT BASECONT  ASSIGN  TO  UT-S-BASECONT
                            ORGANIZATION IS SEQUENTIAL

           SELECT REJECLIE  ASSIGN  TO  UT-S-REJECLIE
                            ORGANIZATION IS SEQUENTIAL
                            FILE STATUS  IS WRK-FS-REJECLIE.

           SELECT CADIDCLI  ASSIGN  TO  UT-S-CADIDCLI
                            ORGANIZATION IS SEQUENTIAL
                            FILE STATUS  IS WRK-FS-CADIDCLI.

      *===============================================================*
       DATA DIVISION.
      *===============================================================*

      *---------------------------------------------------------------*
       FILE SECTION.
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *     INPUT:     ARQUIVO BASECONT                               *
      *                ORG. SEQUENCIAL  - LRECL = 0007                *
      *---------------------------------------------------------------*
       FD  BASECONT
           RECORDING F
           BLOCK 0
           LABEL RECORD IS STANDARD.

       01  FD-REG-BASECONT                 PIC  X(07).

      *---------------------------------------------------------------*
      *     INPUT:     ARQUIVO REJECLIE                               *
      *                ORG. SEQUENCIAL  - LRECL = 0050 FB             *
      *---------------------------------------------------------------*

       FD  REJECLIE
           RECORDING F
           BLOCK 0
           LABEL RECORD IS STANDARD.

       COPY 'GFCTWAD3'.

      *---------------------------------------------------------------*
      *    OUTPUT:     ARQUIVO CADIDCLI                               *
      *---------------------------------------------------------------*

       FD  CADIDCLI
           RECORDING V
           BLOCK 0
           LABEL RECORD IS STANDARD.

       COPY 'GFCTWAD5'.

      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*

       77   FILLER                 PIC X(25)   VALUE
            'INICIO DA WORKING-STORAGE'.

      *---------------------------------------------------------------*
      *               AREAS   AUXILIARES                              *
      *---------------------------------------------------------------*

       77  WRK-CADACHEQ               PIC  X(008)      VALUE 'CADACHEQ'.
       77  WRK-LIDO2                  PIC  9(012) COMP-3 VALUE ZEROS.
       77  WRK-GRAVA-REJECLIE         PIC  9(012) COMP-3 VALUE ZEROS.
       77  WRK-GRAVA-CADIDCLI         PIC  9(012) COMP-3 VALUE ZEROS.
       77  WRK-FS-BASECONT            PIC  X(002)        VALUE '00'.
       77  WRK-FS-REJECLIE            PIC  X(002)        VALUE '00'.
       77  WRK-FS-CADIDCLI            PIC  X(002)        VALUE '00'.
       77  WRK-EOF-BASECONT           PIC  X(001)        VALUE 'N'.

       77  WRK-BATCH                  PIC  X(08) VALUE  'BATCH'.
       77  WRK-ABERTURA               PIC  X(13) VALUE  'NA ABERTURA'.
       77  WRK-LEITURA                PIC  X(13) VALUE  'NA LEITURA'.
       77  WRK-GRAVACAO               PIC  X(13) VALUE  'NA GRAVACAO'.

       01  FILLER.
         05    WRK-CHAVE-BASECONT        PIC  9(12) VALUE ZEROS.
         05    FILLER REDEFINES WRK-CHAVE-BASECONT.
            10 WRK-AGENCIA-AGC           PIC  9(05).
            10 WRK-CONTA-AGC             PIC  9(07).

         05    WRK-CHAVE-CADACHEQ        PIC  9(12) VALUE ZEROS.
         05    FILLER REDEFINES WRK-CHAVE-CADACHEQ.
            10 WRK-CONTA-CHQ             PIC  9(07).

      *---------------------------------------------------------------*
      *                   AREA DO ARQUIVO BASECONT                    *
      *---------------------------------------------------------------*

       01  WRK-BASECONT.
           05  WRK-BAS-AGENCIA          PIC  9(05) COMP-3 VALUE ZEROS.
           05  WRK-BAS-CONTA            PIC  9(07) COMP-3 VALUE ZEROS.

      *---------------------------------------------------------------*
      *                   AREA DO ARQUIVO CADACHEQ                    *
      *---------------------------------------------------------------*

       COPY I#CHEQBN.

       01  WRK-CEP                       PIC  9(09) VALUE ZEROS.
       01  FILLER      REDEFINES         WRK-CEP.
           05  WRK-CEP-ZERO              PIC  9(01).
           05  WRK-CEP-NUMERO            PIC  9(05).
           05  WRK-CEP-SUFIXO            PIC  9(03).
      *---------------------------------------------------------------*
      *              AREAS AUXILIAR PARA MSGS DE ERRO                 *
      *---------------------------------------------------------------*

       01  WRK-MENSAGEM-ERRO.
           03  FILLER                  PIC  X(09) VALUE '*** ERRO '.
           03  WRK-OPERACAO            PIC  X(13) VALUE SPACES.
           03  FILLER                  PIC  X(12) VALUE ' DO ARQUIVO '.
           03  WRK-NOME-ARQUIVO        PIC  X(08) VALUE SPACES.
           03  FILLER                  PIC  X(17) VALUE
               ' - FILE STATUS = '.
           03  WRK-FS                  PIC  X(02) VALUE SPACES.
           03  FILLER                  PIC  X(04) VALUE ' ***'.

      *---------------------------------------------------------------*
      *          AREA POOL7100                                        *
      *---------------------------------------------------------------*

       COPY POL7100C.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

       COPY GFCTWAD0.

      *---------------------------------------------------------------*
      *    CAMPOS AUXILIARES UTILIZADOS NA POOL7600                   *
      *---------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DT-JULIANA          PIC  9(05)  COMP-3  VALUE ZEROS.
           03  WRK-DT-AAMMDD           PIC  9(07)  COMP-3  VALUE ZEROS.
           03  WRK-DT-AAAAMMDD         PIC  9(09)  COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSS           PIC  9(07)  COMP-3  VALUE ZEROS.
           03  WRK-TI-HHMMSSMMMMMM     PIC  9(13)  COMP-3  VALUE ZEROS.
           03  WRK-TIMESTAMP           PIC  X(20)          VALUE SPACES.
           03  FILLER                  REDEFINES        WRK-TIMESTAMP.
               05  WRK-DATA.
                   07  WRK-ANO         PIC  9(04).
                   07  WRK-MES         PIC  9(02).
                   07  WRK-DIA         PIC  9(02).
               05  WRK-HORA            PIC  9(02).
               05  WRK-SEGUNDO         PIC  9(02).
               05  FILLER              PIC  9(06).

       01   FILLER                 PIC X(25)   VALUE
            'FIM DA WORKING-STORAGE'.
             EJECT

      *===============================================================*
       PROCEDURE DIVISION.
      *===============================================================*
      *---------------------------------------------------------------*
       0000-LOGICA-PRINCIPAL   SECTION.
      *---------------------------------------------------------------*

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESSA  UNTIL  WRK-EOF-BASECONT EQUAL 'S'.

           PERFORM 9000-FINALIZA.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1000-INICIO   SECTION.
      *---------------------------------------------------------------*

           OPEN   INPUT  BASECONT
                 OUTPUT  CADIDCLI
                         REJECLIE.

           MOVE    WRK-ABERTURA   TO WRK-OPERACAO.
           PERFORM 1100-CONSISTIR-FS.

           PERFORM 1200-PRIMEIRA-LEITURA.

           PERFORM 1400-OBTER-DATA-HORA.

           MOVE  'GFCT2825'   TO    WRK-PROGNAME.

      *---------------------------------------------------------------*
       1000-999-FIM-INICIO.    EXIT.
      *---------------------------------------------------------------*
       1100-CONSISTIR-FS       SECTION.
      *---------------------------------------------------------------*

           PERFORM 1300-TRATAR-FS-BASECONT.
           PERFORM 1301-TRATAR-FS-REJECLIE.
           PERFORM 1302-TRATAR-FS-CADIDCLI.

      *---------------------------------------------------------------*
       1100-999-FIM.           EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1200-PRIMEIRA-LEITURA   SECTION.
      *---------------------------------------------------------------*

      *---> LEITURA DO PRIMEIRO REGISTRO DO ARQUIVO CADACHEQ

           PERFORM 3000-LER-CADACHEQ.

           IF  RETURN-CODE    EQUAL    4
               DISPLAY '************ GFCT2825 ***************'
               DISPLAY '*                                   *'
               DISPLAY '*                                   *'
               DISPLAY '*     PROCESSAMENTO ENCERRADO       *'
               DISPLAY '*                                   *'
               DISPLAY '************ GFCT2825 ***************'
           END-IF.

      *---> LEITURA DO PRIMEIRO REGISTRO DO ARQUIVO BASECONT

           PERFORM 4000-LER-BASECONT.

           IF  WRK-EOF-BASECONT   EQUAL   'S'
               DISPLAY '************ GFCT2825 ***************'
               DISPLAY '*                                   *'
               DISPLAY '*    ARQUIVO  BASECONT  VAZIO       *'
               DISPLAY '*                                   *'
               DISPLAY '*     PROCESSAMENTO ENCERRADO       *'
               DISPLAY '*                                   *'
               DISPLAY '************ GFCT2825 ***************'
           END-IF.

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       1300-TRATAR-FS-BASECONT   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-BASECONT NOT EQUAL '00'
              DISPLAY '************** GFCT2825 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            BASECONT               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-BASECONT
                                                 '         *'
              DISPLAY '************** GFCT2825 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'BASECONT'        TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-BASECONT   TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1300-999-FIM. EXIT.
      *---------------------------------------------------------------*
       1301-TRATAR-FS-REJECLIE   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-REJECLIE  NOT EQUAL '00' AND '04'
              DISPLAY '************** GFCT2825 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            REJECLIE               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-REJECLIE
                                                 '         *'
              DISPLAY '************** GFCT2825 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'REJECLIE'        TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-REJECLIE   TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1301-999-FIM. EXIT.
      *---------------------------------------------------------------*
       1302-TRATAR-FS-CADIDCLI   SECTION.
      *---------------------------------------------------------------*

           IF WRK-FS-CADIDCLI  NOT EQUAL '00' AND '04'
              DISPLAY '************** GFCT2825 *************'
              DISPLAY '*   ERRO ' WRK-OPERACAO  ' DO ARQUIVO   *'
              DISPLAY '*            CADIDCLI               *'
              DISPLAY '*         FILE STATUS =  ' WRK-FS-CADIDCLI
                                                 '         *'
              DISPLAY '************** GFCT2825 *************'
              MOVE  'APL'             TO      ERR-TIPO-ACESSO
              MOVE  'CADIDCLI'        TO      WRK-NOME-ARQUIVO
              MOVE  WRK-FS-CADIDCLI   TO      WRK-FS
              MOVE  WRK-MENSAGEM-ERRO TO      ERR-TEXTO
              PERFORM  9999-ROTINA-ERRO
           END-IF.

      *---------------------------------------------------------------*
       1302-999-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

           CALL  'POOL7600'  USING  WRK-DATA-HORA.

           STRING WRK-DIA '/'
                  WRK-MES '/'
                  WRK-ANO
           DELIMITED BY SIZE           INTO WRK-DATAMOV.

           STRING WRK-HORA   ':'
                  WRK-MINUTO ':'
                  WRK-SEGUNDO
           DELIMITED BY SIZE           INTO WRK-HORAMOV.

      *---------------------------------------------------------------*
       1400-999-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       2000-PROCESSA    SECTION.
      *---------------------------------------------------------------*


           IF    WRK-CHAVE-BASECONT   LESS   WRK-CHAVE-CADACHEQ
                 MOVE      201          TO   D3-CHV-CODIGO
                 PERFORM   7000-GRV-REJECLIE
                 PERFORM   8100-GRV-CADA-REJ
                 PERFORM   4000-LER-BASECONT
           ELSE
             IF  WRK-CHAVE-BASECONT   EQUAL  WRK-CHAVE-CADACHEQ
                 PERFORM   8000-GRV-CADADCLI
                 PERFORM   3000-LER-CADACHEQ
                 PERFORM   4000-LER-BASECONT
             ELSE
                 PERFORM   3000-LER-CADACHEQ
             END-IF
           END-IF.

      *---------------------------------------------------------------*
       2000-999-FIM-PROCESSA. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       3000-LER-CADACHEQ   SECTION.

           CALL  'POOL0300'      USING     WRK-CADACHEQ
                                           REG-CADCECHEQ.

           IF   RETURN-CODE       EQUAL    4
                MOVE   999999999999  TO    WRK-CHAVE-CADACHEQ
                GO TO  3000-999-FIM-LER
           ELSE
                ADD 1   TO  WRK-LIDO1
           END-IF.

      *---> MONTA CHAVE

           MOVE   CECHEQ-AGENCIA     TO    WRK-AGENCIA-CHQ
           MOVE   CECHEQ-CONTA       TO    WRK-CONTA-CHQ.

      *---------------------------------------------------------------*
       3000-999-FIM-LER.           EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       4000-LER-BASECONT           SECTION.

           READ BASECONT  INTO  WRK-BASECONT.

           IF  WRK-FS-BASECONT      EQUAL   '10'
               MOVE   999999999999  TO       WRK-CHAVE-BASECONT
               MOVE   'S'           TO       WRK-EOF-BASECONT
               GO      TO   4000-999-FIM-LER-BASECONT
           END-IF.

           MOVE    WRK-LEITURA      TO       WRK-OPERACAO.
           PERFORM 1300-TRATAR-FS-BASECONT.

      *---> MONTA CHAVE

           ADD         1            TO       WRK-LIDO2.
           MOVE   WRK-BAS-AGENCIA   TO       WRK-AGENCIA-AGC.
           MOVE   WRK-BAS-CONTA     TO       WRK-CONTA-AGC.

      *---------------------------------------------------------------*
       4000-999-FIM-LER-BASECONT.  EXIT.
      *---------------------------------------------------------------*
       7000-GRV-REJECLIE           SECTION.
      *---------------------------------------------------------------*

           MOVE  WRK-BAS-AGENCIA   TO  D3-CHV-AGENCIA.
           MOVE  WRK-BAS-CONTA     TO  D3-CHV-CONTA.
           MOVE  SPACES            TO  D3-DAD-RESTO.

           WRITE D3-REGISTRO.

           MOVE  WRK-GRAVACAO   TO   WRK-OPERACAO.
           PERFORM 1301-TRATAR-FS-REJECLIE.

           ADD   1              TO   WRK-GRAVA-REJECLIE.

      *---------------------------------------------------------------*
       7000-999-FIM-GRAVA-REGISTRO. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       8000-GRV-CADADCLI   SECTION.
      *---------------------------------------------------------------*


           IF  ( CECHEQ-NOME-TAM40   EQUAL   SPACES )  OR
               ( CECHEQ-ENDERECO     EQUAL   SPACES )
                 MOVE     202           TO   D3-CHV-CODIGO
                 PERFORM  7000-GRV-REJECLIE
                 PERFORM  8100-GRV-CADA-REJ
                 GO TO    8000-999-FIM-GRAVA-REGISTRO
           END-IF.

      *---> FORMATA CAMPO DO ARQUIVO CADIDCLI

           MOVE  CECHEQ-ENDERECO   TO  D5-CAD-ENDERECO
           MOVE  CECHEQ-NOME-TAM40 TO  D5-CAD-NOME

           MOVE  CECHEQ-AGENCIA    TO  D5-CAD-AGENCIA
           MOVE  CECHEQ-CONTA      TO  D5-CAD-CONTA
           MOVE      1000          TO  D5-CAD-ASSUNTO
           MOVE       001          TO  D5-CAD-SEGMENTO
           MOVE       001          TO  D5-CAD-SEQUENCIA
           MOVE     SPACES         TO  D5-CAD-SEGMENTACAO
                                       D5-CAD-TIPO
           MOVE  CECHEQ-FONE       TO  D5-CAD-FONE
           MOVE  CECHEQ-DDD-FAX    TO  D5-CAD-DDD-FAX
           MOVE  CECHEQ-FAX-FONE   TO  D5-CAD-FAX
           MOVE  CECHEQ-PRINCIPAL  TO  D5-CAD-NRO-CGC-CPF
           MOVE  CECHEQ-FILIAL     TO  D5-CAD-FIL-CGC-CPF
           MOVE  CECHEQ-CTR        TO  D5-CAD-CTR-CGC-CPF
           MOVE  CECHEQ-NOVO-CEP   TO  WRK-CEP-NUMERO
                                       D5-CAD-CEP-NRO.

           IF  CECHEQ-COMPL-CEP       NOT    NUMERIC
               MOVE  ZEROS            TO     WRK-CEP-SUFIXO
                                             D5-CAD-CEP-COMPL
           ELSE
               MOVE  CECHEQ-COMPL-CEP TO     WRK-CEP-SUFIXO
                                             D5-CAD-CEP-COMPL
           END-IF.

           MOVE  ZEROS          TO   WRK-CEP-ZERO.
           MOVE  WRK-CEP        TO   D5-CAD-CEP-CHAVE.

           WRITE REG-CADASTRO.
           MOVE  WRK-GRAVACAO   TO   WRK-OPERACAO.
           PERFORM 1302-TRATAR-FS-CADIDCLI.

           ADD   1              TO   WRK-GRAVA-CADIDCLI.

      *---------------------------------------------------------------*
       8000-999-FIM-GRAVA-REGISTRO.  EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       8100-GRV-CADA-REJ             SECTION.
      *---------------------------------------------------------------*

      *---> FORMATA CAMPO DO ARQUIVO CADIDCLI

           MOVE  SPACES            TO  D5-CAD-DADOS.

           MOVE  WRK-BAS-AGENCIA   TO  D5-CAD-AGENCIA
           MOVE  WRK-BAS-CONTA     TO  D5-CAD-CONTA
           MOVE      1000          TO  D5-CAD-ASSUNTO
           MOVE       001          TO  D5-CAD-SEGMENTO
           MOVE       001          TO  D5-CAD-SEQUENCIA
           MOVE      ZEROS         TO  D5-CAD-CEP-CHAVE.

           WRITE REG-CADASTRO.

           MOVE  WRK-GRAVACAO   TO   WRK-OPERACAO.
           PERFORM 1302-TRATAR-FS-CADIDCLI.

           ADD   1              TO   WRK-GRAVA-CADIDCLI.

      *---------------------------------------------------------------*
       8100-999-GRV-CADA-REJ.        EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       9000-FINALIZA                 SECTION.
      *---------------------------------------------------------------*

           CLOSE  CADIDCLI
                  REJECLIE
                  BASECONT.

           MOVE    WRK-GRAVACAO   TO   WRK-OPERACAO.

      *---> RESUMO DOS TOTAIS

           PERFORM  9100-EXIBIR-DISP-ESTATIST.

           STOP RUN.

      *---------------------------------------------------------------*
       9000-999-FIM-FINALIZA. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       9100-EXIBIR-DISP-ESTATIST   SECTION.
      *---------------------------------------------------------------*

           DISPLAY WRK-DISP1.
           DISPLAY WRK-DISP2.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP4.
           DISPLAY WRK-DISP5.
           DISPLAY WRK-DISP6.
           DISPLAY WRK-DISP3.
           DISPLAY WRK-DISP7.
           DISPLAY WRK-DISP8.

           MOVE WRK-LIDO1              TO WRK-QTDEARQ.
           MOVE 'CADACHEQ'             TO WRK-DDNAME.
           MOVE 'I'                    TO WRK-I-O.
           MOVE 'ARQUIVO DE CLIENTES        '
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.

           MOVE WRK-LIDO2              TO WRK-QTDEARQ.
           MOVE 'BASECONT'             TO WRK-DDNAME.
           MOVE 'I'                    TO WRK-I-O.
           MOVE 'ARQ PUBLICO DE VALIDACAO   '
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.

           MOVE WRK-GRAVA-CADIDCLI     TO WRK-QTDEARQ.
           MOVE 'CADIDCLI'             TO WRK-DDNAME.
           MOVE 'O'                    TO WRK-I-O.
           MOVE 'CADASTRO IDENTIF. CLIENTES '
           DISPLAY WRK-DISP9.

           MOVE WRK-GRAVA-REJECLIE     TO WRK-QTDEARQ.
           MOVE 'REJECLIE'             TO WRK-DDNAME.
           MOVE 'O'                    TO WRK-I-O.
           MOVE 'CADASTRO REJEIT. CLIENTES  '
                                       TO WRK-DESCARQ.
           DISPLAY WRK-DISP9.

           DISPLAY WRK-DISP1.

      *---------------------------------------------------------------*
       9100-999-FIM. EXIT.
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
       9999-ROTINA-ERRO               SECTION.
      *---------------------------------------------------------------*

           MOVE  'GFCT2825'            TO      ERR-PGM.

           CALL  'POOL7100'            USING   WRK-BATCH

           GOBACK.

      *----------------------------------------------------------------*
       9999-999-FIM. EXIT.
      *----------------------------------------------------------------*

