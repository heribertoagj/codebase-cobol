      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
      *================================================================*

       PROGRAM-ID. GFCT6855.
       AUTHOR. MARCIO RUI.
      *================================================================*
      *                B A N C O  B R A D E S C O  -  S . A            *
      *----------------------------------------------------------------*
      *                                                                *
      *    PROGRAMA     : GFCP6855                                     *
      *    PROGRAMADOR  : MARCIO ALEXANDRO RUI                         *
      *    ANALISTA     : MARCIO ALEXANDRO RUI                         *
      *    DATA         : 04/09/2019                                   *
      *                                                                *
      *    OBJETIVO     :                                              *
      *      GERAR ARQUIVO DE SEGMENTO ATUALIZADO E EMITIR RELATORIO   *
      *      DAS CONTAS QUE SAIRAM DO ARQUIVO DO SERVICO 66.           *
      *      (CONSULTORES AVON)                                        *
      *                                                                *
      *    ARQUIVOS     :                                              *
      *      DDNAME                                INCLUDE/BOOK        *
      *      SEGMEATU                                GFCTW003          *
      *      SEGMEANT                                GFCTW003          *
      *      SEGMOVTO                                GFCTW003          *
      *      RELATO                                  --------          *
      *                                                                *
      *    MODULOS CHAMADOS:                                           *
      *      POOL7100 - CANCELAMENTO DO PROGRAMA.                      *
      *      POOL7600 - OBTEM DATA E HORA DE SISTEMA.                  *
      *                                                                *
      *================================================================*

      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*

       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*

       FILE-CONTROL.

           SELECT SEGMEATU ASSIGN      TO UT-S-SEGMEATU
                      FILE STATUS      IS WRK-FS-SEGMEATU.

           SELECT SEGMEANT ASSIGN      TO UT-S-SEGMEANT
                      FILE STATUS      IS WRK-FS-SEGMEANT.

           SELECT SEGMOVTO ASSIGN      TO UT-S-SEGMOVTO
                      FILE STATUS      IS WRK-FS-SEGMOVTO.

           SELECT RELATO   ASSIGN      TO UT-S-RELATO
                      FILE STATUS      IS WRK-FS-RELATO.

      *================================================================*
       DATA                            DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO CONTA EM TEST DRIVE - PROJETO AVON        *
      *              MOVIMENTO DATA ATUAL                              *
      *              ORG. SEQUENCIAL   -   LRECL = 200                 *
      *----------------------------------------------------------------*

       FD  SEGMEATU
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.

      *----------------------------------------------------------------*
      *    INPUT  :  ARQUIVO CONTA EM TEST DRIVE - PROJETO AVON        *
      *              MOVIMENTO DATA ANTERIOR                           *
      *              ORG. SEQUENCIAL   -   LRECL = 200                 *
      *----------------------------------------------------------------*

       FD  SEGMEANT
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.

      *----------------------------------------------------------------*
      *    OUTPUT :  ARQUIVO CONTA EM TEST DRIVE - PROJETO AVON        *
      *              MOVIMENTO ATUALIZADO                              *
      *              ORG. SEQUENCIAL   -   LRECL = 200                 *
      *----------------------------------------------------------------*

       FD  SEGMOVTO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       COPY GFCTW003.

      *----------------------------------------------------------------*
      *    OUTPUT :  RELATORIO DAS CONTAS QUE PERDERAM A MARCACAO      *
      *              DO SERVICO 66 (REVENDEDORES AVON)                 *
      *              ORG. SEQUENCIAL   -   LRECL = 133                 *
      *----------------------------------------------------------------*

       FD  RELATO
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FD-REG-RELATO                  PIC  X(133).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*  INICIO DA WORKING GFCT6855  *'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*         ACUMULADORES         *'.
      *----------------------------------------------------------------*

       77  ACU-LIDOS-SEGMEATU          PIC  9(09) COMP-3 VALUE ZEROS.
       77  ACU-LIDOS-SEGMEANT          PIC  9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-SEGMOVTO          PIC  9(09) COMP-3 VALUE ZEROS.
       77  ACU-GRAVA-RELATO            PIC  9(09) COMP-3 VALUE ZEROS.
       77  ACU-PAGINA                  PIC  9(07) COMP-3 VALUE ZEROS.
       77  ACU-LINHAS                  PIC  9(03) COMP-3 VALUE 99.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     TESTE DE FILE-STATUS     *'.
      *----------------------------------------------------------------*

       77  WRK-FS-SEGMEATU             PIC  X(02)        VALUE SPACES.
       77  WRK-FS-SEGMEANT             PIC  X(02)        VALUE SPACES.
       77  WRK-FS-SEGMOVTO             PIC  X(02)        VALUE SPACES.
       77  WRK-FS-RELATO               PIC  X(02)        VALUE SPACES.

       77  WRK-ABERTURA                PIC  X(13)        VALUE
           ' NA ABERTURA'.
       77  WRK-LEITURA                 PIC  X(13)        VALUE
           ' NA  LEITURA'.
       77  WRK-GRAVACAO                PIC  X(13)        VALUE
           ' NA GRAVACAO'.
       77  WRK-FECHAMENTO              PIC  X(13)        VALUE
           'NO FECHAMENTO'.

      *----------------------------------------------------------------*
       77  FILLER                      PIC  X(32)        VALUE
           '*     VARIAVEIS AUXILIARES     *'.
      *----------------------------------------------------------------*

       77  WRK-BATCH                   PIC  X(08)        VALUE 'BATCH'.
       77  WRK-MASCARA                 PIC  ZZZ.ZZZ.ZZ9  VALUE ZEROS.

       01  WRK-CHAVE-SEGMEATU.
           03  WRK-AGENCIA-ATU         PIC  9(05)        VALUE ZEROS.
           03  WRK-CONTA-ATU           PIC  9(07)        VALUE ZEROS.
           03  WRK-SEGMENTO-ATU        PIC  9(03)        VALUE ZEROS.

       01  WRK-CHAVE-SEGMEANT.
           03  WRK-AGENCIA-ANT         PIC  9(05)        VALUE ZEROS.
           03  WRK-CONTA-ANT           PIC  9(07)        VALUE ZEROS.
           03  WRK-SEGMENTO-ANT        PIC  9(03)        VALUE ZEROS.

       01  WRK-DT-INCL                 PIC  9(08)        VALUE ZEROS.
       01  WRK-DT-INCL-R               REDEFINES WRK-DT-INCL.
           03  WRK-ANO-INC             PIC  9(04).
           03  WRK-MES-INC             PIC  9(02).
           03  WRK-DIA-INC             PIC  9(02).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*     MSG ERRO FILE-STATUS     *'.
      *----------------------------------------------------------------*

       01  WRK-MSG-FS-ERRO.
           03  FILLER                  PIC  X(08)        VALUE SPACES.
           03  FILLER                  PIC  X(02)        VALUE '* '.
           03  FILLER                  PIC  X(05)        VALUE 'ERRO'.
           03  WRK-OPERACAO            PIC  X(13)        VALUE SPACES.
           03  FILLER                  PIC  X(12)        VALUE
              ' DO ARQUIVO '.
           03  WRK-NOME-ARQ            PIC  X(08)        VALUE SPACES.
           03  FILLER                  PIC  X(17)        VALUE
               ' - FILE-STATUS = '.
           03  WRK-FILE-STATUS         PIC  X(02)        VALUE SPACES.
           03  FILLER                  PIC  X(02)        VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)         VALUE
           '*      AREA DA POOL7600        *'.
      *----------------------------------------------------------------*

       01  WRK-DATA-HORA.
           03  WRK-DATA-JULIANA        PIC  9(05) COMP-3 VALUE ZEROS.
           03  WRK-DATA-AAMMDD         PIC  9(07) COMP-3 VALUE ZEROS.
           03  WRK-DATA-AAAAMMDD       PIC  9(09) COMP-3 VALUE ZEROS.
           03  WRK-DATA-HHMMSS         PIC  9(07) COMP-3 VALUE ZEROS.
           03  WRK-DATA-HHMMSSMMMMMM   PIC  9(13) COMP-3 VALUE ZEROS.
           03  WRK-TIMESTAMP.
               05  WRK-ANO             PIC  9(04)        VALUE ZEROS.
               05  WRK-MES             PIC  9(02)        VALUE ZEROS.
               05  WRK-DIA             PIC  9(02)        VALUE ZEROS.
               05  WRK-HORA            PIC  9(02)        VALUE ZEROS.
               05  WRK-MINUTO          PIC  9(02)        VALUE ZEROS.
               05  WRK-SEGUNDO         PIC  9(02)        VALUE ZEROS.
               05  FILLER              PIC  X(06)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   AREA DE CABECALHO RELATO   *'.
      *----------------------------------------------------------------*

       01  CB1-REL1.
           03  CB1-CARRO               PIC  X(01)        VALUE '1'.
           03  FILLER                  PIC  X(08)        VALUE
               'GFCT6855'.
           03  FILLER                  PIC  X(48)        VALUE SPACES.
           03  FILLER                  PIC  X(63)        VALUE
               'BANCO BRADESCO S.A'.
           03  FILLER                  PIC  X(07)        VALUE 'PAGINA'.
           03  CB1-PAGINA              PIC  ZZZZZ9       VALUE ZEROS.

       01  CB2-REL1.
           03  CB2-CARRO               PIC  X(01)        VALUE ' '.
           03  CB2-DATA                PIC  X(10)        VALUE SPACES.
           03  FILLER                  PIC  X(37)        VALUE SPACES.
           03  FILLER                  PIC  X(72)        VALUE
               'RELACAO DAS CONTAS QUE PERDERAM A MARCACAO.'.
           03  CB2-HORA                PIC  X(08)        VALUE SPACES.
           03  FILLER                  PIC  X(05)        VALUE SPACES.

       01  CB3-REL1.
           03  CB3-CARRO               PIC  X(01)        VALUE '0'.
           03  FILLER                  PIC  X(06)        VALUE 'BANCO'.
           03  FILLER                  PIC  X(08)        VALUE
               'AGENCIA'.
           03  FILLER                  PIC  X(08)        VALUE 'CONTA'.
           03  FILLER                  PIC  X(06)        VALUE 'RAZAO'.
           03  FILLER                  PIC  X(07)        VALUE 'TP.CTA'.
           03  FILLER                  PIC  X(09)        VALUE
               'CESTA   '.
           03  FILLER                  PIC  X(32)        VALUE
               'CLIENTE'.
           03  FILLER                  PIC  X(21)        VALUE
               'ABERTURA'.
           03  FILLER                  PIC  X(12)        VALUE 'POSTO'.
           03  FILLER                  PIC  X(12)        VALUE
               'CPF/CNPJ'.
           03  FILLER                  PIC  X(11)        VALUE
               'DT.INCLUSAO'.

       01  CB4-REL1.
           03  CB4-CARRO               PIC  X(01)        VALUE ' '.
           03  FILLER                  PIC  X(132)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*   AREA DE DETALHE RELATO     *'.
      *----------------------------------------------------------------*

       01  LD1-REL1.
           03  LD1-CARRO               PIC  X(01)        VALUE ' '.
           03  FILLER                  PIC  X(01)        VALUE SPACES.
           03  LD1-BANCO               PIC  9(03)        VALUE ZEROS.
           03  FILLER                  PIC  X(03)        VALUE SPACES.
           03  LD1-AGENCIA             PIC  9(05)        VALUE ZEROS.
           03  FILLER                  PIC  X(02)        VALUE SPACES.
           03  LD1-CONTA               PIC  9(07)        VALUE ZEROS.
           03  FILLER                  PIC  X(01)        VALUE SPACES.
           03  LD1-RAZAO1              PIC  9(05)        VALUE ZEROS.
           03  FILLER                  PIC  X(01)        VALUE SPACES.
           03  LD1-TIPO-CONTA          PIC  9(03)        VALUE ZEROS.
           03  FILLER                  PIC  X(04)        VALUE SPACES.
           03  LD1-CESTA               PIC  9(05)        VALUE ZEROS.
           03  FILLER                  PIC  X(03)        VALUE SPACES.
           03  LD1-CLIENTE             PIC  X(40)        VALUE SPACES.
           03  FILLER                  PIC  X(01)        VALUE SPACES.
           03  LD1-ABERTURA            PIC  X(10)        VALUE SPACES.
           03  FILLER                  PIC  X(03)        VALUE SPACES.
           03  LD1-POSTO               PIC  9(03)        VALUE ZEROS.
           03  FILLER                  PIC  X(02)        VALUE SPACES.
ST25X6*    03  LD1-NUM-CPF-CNPJ        PIC  999999999    VALUE ZEROS.
           03  LD1-NUM-CPF-CNPJ        PIC  X(009)       VALUE SPACES.
           03  FILLER                  PIC  X(01)        VALUE '-'.
ST25X6*    03  LD1-FIL-CPF-CNPJ        PIC  99999        VALUE ZEROS.
           03  LD1-FIL-CPF-CNPJ        PIC  X(004)       VALUE SPACES.
           03  FILLER                  PIC  X(01)        VALUE '/'.
ST25X6*    03  LD1-CTRL-CPF-CNPJ       PIC  99           VALUE ZEROS.
           03  LD1-CTRL-CPF-CNPJ       PIC  X(002)       VALUE SPACES.
           03  FILLER                  PIC  X(02)        VALUE SPACES.
           03  LD1-DT-INCLUSAO         PIC  X(10)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*       AREA DA POOL7100       *'.
      *----------------------------------------------------------------*

       COPY POL7100C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(32)        VALUE
           '*    FIM DA WORKING GFCT6855   *'.
      *----------------------------------------------------------------*

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
       0000-ROTINA-PRINCIPAL          SECTION.
      *----------------------------------------------------------------*

           PERFORM 1000-INICIAR-PROGRAMA.

           PERFORM 2000-VERIFICAR-VAZIO.

           PERFORM 3000-PROCESSAR     UNTIL
                   WRK-CHAVE-SEGMEATU  EQUAL HIGH-VALUES AND
                   WRK-CHAVE-SEGMEANT  EQUAL HIGH-VALUES.

           PERFORM 4000-FINALIZAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA          SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  SEGMEATU
                       SEGMEANT
                OUTPUT SEGMOVTO
                       RELATO.

           MOVE WRK-ABERTURA           TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           CALL 'POOL7600'             USING WRK-DATA-HORA.

           STRING WRK-DIA '/' WRK-MES '/' WRK-ANO
                                       DELIMITED BY SIZE
                                       INTO CB2-DATA.

           STRING WRK-HORA ':' WRK-MINUTO ':' WRK-SEGUNDO
                                       DELIMITED BY SIZE
                                       INTO CB2-HORA.

      *----------------------------------------------------------------*
       1000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-TESTAR-FILE-STATUS        SECTION.
      *----------------------------------------------------------------*

           PERFORM 1110-TESTAR-FS-SEGMEATU.

           PERFORM 1120-TESTAR-FS-SEGMEANT.

           PERFORM 1130-TESTAR-FS-SEGMOVTO.

           PERFORM 1140-TESTAR-FS-RELATO.

      *----------------------------------------------------------------*
       1100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-TESTAR-FS-SEGMEATU        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGMEATU         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SEGMEATU'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SEGMEATU    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1120-TESTAR-FS-SEGMEANT        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGMEANT         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SEGMEANT'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SEGMEANT    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1120-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1130-TESTAR-FS-SEGMOVTO        SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-SEGMOVTO         NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'SEGMOVTO'         TO WRK-NOME-ARQ
               MOVE WRK-FS-SEGMOVTO    TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1130-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1140-TESTAR-FS-RELATO          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-FS-RELATO           NOT EQUAL '00'
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE 'RELATO'           TO WRK-NOME-ARQ
               MOVE WRK-FS-RELATO      TO WRK-FILE-STATUS
               MOVE WRK-MSG-FS-ERRO    TO ERR-TEXTO
               PERFORM 9999-ROTINA-ERRO
           END-IF.

      *----------------------------------------------------------------*
       1140-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-VERIFICAR-VAZIO           SECTION.
      *----------------------------------------------------------------*

           PERFORM 2100-LER-SEGMEATU.

           IF  WRK-FS-SEGMEATU         EQUAL '10'
               DISPLAY '************** GFCT6855 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*        ARQUIVO SEGMEATU VAZIO       *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT6855 ***************'
           END-IF.

           PERFORM 2200-LER-SEGMEANT.

           IF  WRK-FS-SEGMEANT         EQUAL '10'
               DISPLAY '************** GFCT6855 ***************'
               DISPLAY '*                                     *'
               DISPLAY '*        ARQUIVO SEGMEANT VAZIO       *'
               DISPLAY '*                                     *'
               DISPLAY '************** GFCT6855 ***************'
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-LER-SEGMEATU              SECTION.
      *----------------------------------------------------------------*

           READ SEGMEATU.

           IF  WRK-FS-SEGMEATU         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-SEGMEATU
               GO                      TO 2100-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1110-TESTAR-FS-SEGMEATU.

           MOVE GFCT0003-CJUNC-DEPDC OF SEGMEATU
                                       TO WRK-AGENCIA-ATU.
           MOVE GFCT0003-CCTA-CORR   OF SEGMEATU
                                       TO WRK-CONTA-ATU.
AQUI***#####OVE SEG-SEGMENTO         OF SEGMEATU
AQUI***#####                           TO WRK-SEGMENTO-ATU.

           ADD 1                       TO ACU-LIDOS-SEGMEATU.

      *----------------------------------------------------------------*
       2100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-LER-SEGMEANT              SECTION.
      *----------------------------------------------------------------*

           READ SEGMEANT.

           IF  WRK-FS-SEGMEANT         EQUAL '10'
               MOVE HIGH-VALUES        TO WRK-CHAVE-SEGMEANT
               GO                      TO 2200-99-FIM
           END-IF.

           MOVE WRK-LEITURA            TO WRK-OPERACAO.

           PERFORM 1120-TESTAR-FS-SEGMEANT.

           MOVE GFCT0003-CJUNC-DEPDC OF SEGMEANT
                                       TO WRK-AGENCIA-ANT.
           MOVE GFCT0003-CCTA-CORR   OF SEGMEANT
                                       TO WRK-CONTA-ANT.
AQUI***#####OVE SEG-SEGMENTO OF SEGMEANT
AQUI***#####                           TO WRK-SEGMENTO-ANT.

           ADD 1                       TO ACU-LIDOS-SEGMEANT.

      *----------------------------------------------------------------*
       2200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR                 SECTION.
      *----------------------------------------------------------------*

           IF  WRK-CHAVE-SEGMEATU      EQUAL WRK-CHAVE-SEGMEANT
               PERFORM 3100-FORMATAR-CHAVE-IGUAL
           ELSE
               IF  WRK-CHAVE-SEGMEATU  LESS WRK-CHAVE-SEGMEANT
                   PERFORM 3200-FORMATAR-CHAVE-MENOR
               ELSE
                   PERFORM 3300-FORMATAR-CHAVE-MAIOR
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       3000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3100-FORMATAR-CHAVE-IGUAL      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0003-REGISTRO OF SEGMEATU
                                       TO GFCT0003-REGISTRO OF SEGMOVTO.

           PERFORM 3110-GRAVAR-SEGMOVTO.

           PERFORM 2100-LER-SEGMEATU.

           PERFORM 2200-LER-SEGMEANT.

      *----------------------------------------------------------------*
       3100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3110-GRAVAR-SEGMOVTO           SECTION.
      *----------------------------------------------------------------*

           WRITE GFCT0003-REGISTRO OF SEGMOVTO.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1130-TESTAR-FS-SEGMOVTO.

           ADD 1                       TO ACU-GRAVA-SEGMOVTO.

      *----------------------------------------------------------------*
       3110-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3200-FORMATAR-CHAVE-MENOR      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0003-REGISTRO OF SEGMEATU
                                       TO GFCT0003-REGISTRO OF SEGMOVTO.

           PERFORM 3110-GRAVAR-SEGMOVTO.

           PERFORM 2100-LER-SEGMEATU.

      *----------------------------------------------------------------*
       3200-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3300-FORMATAR-CHAVE-MAIOR      SECTION.
      *----------------------------------------------------------------*

           MOVE GFCT0003-REGISTRO OF SEGMEANT
                                       TO GFCT0003-REGISTRO OF SEGMOVTO.

RU0620     IF GFCT0003-CSERVICO-SCCE OF SEGMEANT NOT EQUAL 74
              MOVE ZEROS                  TO GFCT0003-CESTA-SERVICO
                                                            OF SEGMOVTO
RU0620     END-IF

           PERFORM 3110-GRAVAR-SEGMOVTO.

           PERFORM 3310-IMPRIMIR-RELATO.

           PERFORM 2200-LER-SEGMEANT.

      *----------------------------------------------------------------*
       3300-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3310-IMPRIMIR-RELATO           SECTION.
      *----------------------------------------------------------------*

           IF  ACU-LINHAS              GREATER 59
               PERFORM 3311-IMPRIMIR-CABEC
           END-IF.

           MOVE  237                   TO LD1-BANCO.
           MOVE GFCT0003-CJUNC-DEPDC  OF SEGMEANT
                                       TO LD1-AGENCIA.
           MOVE GFCT0003-CCTA-CORR    OF SEGMEANT
                                       TO LD1-CONTA.
           MOVE GFCT0003-RAZAO        OF SEGMEANT
                                       TO LD1-RAZAO1.
           MOVE GFCT0003-CESTA-SERVICO OF SEGMEANT
                                       TO LD1-CESTA.
           MOVE GFCT0003-NOME-CLIENTE  OF SEGMEANT
                                       TO LD1-CLIENTE.
AQUI****####OVE SEG-CID-CLIE         OF SEGMEANT
AQUI****####                           TO LD1-ABERTURA.
AQUI****####OVE SEG-POSTO-SERVICO    OF SEGMEANT
AQUI****####                           TO LD1-POSTO.
           MOVE GFCT0003-CPF-CNPJ     OF SEGMEANT
                                       TO LD1-NUM-CPF-CNPJ.
           MOVE GFCT0003-FILIAL       OF SEGMEANT
                                       TO LD1-FIL-CPF-CNPJ.
           MOVE GFCT0003-CONTROLE     OF SEGMEANT
                                       TO LD1-CTRL-CPF-CNPJ.
           MOVE GFCT0003-DINIC-TST-DRIVE OF SEGMEANT
                                       TO WRK-DT-INCL.

           STRING WRK-DIA-INC '/' WRK-MES-INC '/' WRK-ANO-INC
                                       DELIMITED BY SIZE
                                       INTO LD1-DT-INCLUSAO.

           WRITE FD-REG-RELATO         FROM LD1-REL1.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           PERFORM 1140-TESTAR-FS-RELATO.

           ADD 1                       TO ACU-GRAVA-RELATO
                                          ACU-LINHAS.

      *----------------------------------------------------------------*
       3310-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3311-IMPRIMIR-CABEC            SECTION.
      *----------------------------------------------------------------*

           ADD 1                       TO ACU-PAGINA.
           MOVE ACU-PAGINA             TO CB1-PAGINA.

           MOVE WRK-GRAVACAO           TO WRK-OPERACAO.

           WRITE FD-REG-RELATO         FROM CB1-REL1.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CB2-REL1.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CB3-REL1.
           PERFORM 1140-TESTAR-FS-RELATO.

           WRITE FD-REG-RELATO         FROM CB4-REL1.
           PERFORM 1140-TESTAR-FS-RELATO.

           MOVE 5                      TO ACU-LINHAS.

      *----------------------------------------------------------------*
       3311-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4000-FINALIZAR                 SECTION.
      *----------------------------------------------------------------*

           PERFORM 4100-EMITIR-DISPLAY.

           CLOSE SEGMEATU
                 SEGMEANT
                 SEGMOVTO
                 RELATO.

           MOVE WRK-FECHAMENTO         TO WRK-OPERACAO.

           PERFORM 1100-TESTAR-FILE-STATUS.

           GOBACK.

      *----------------------------------------------------------------*
       4000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       4100-EMITIR-DISPLAY            SECTION.
      *----------------------------------------------------------------*

           MOVE ACU-LIDOS-SEGMEATU     TO WRK-MASCARA.

           DISPLAY '************** GFCT6855 ***************'.
           DISPLAY '*                                     *'.
           DISPLAY '*  TOTAL DE REGISTROS:                *'.
           DISPLAY '*                                     *'.
           DISPLAY '*    LIDOS    SEGMEATU  : ' WRK-MASCARA  ' *'.
           DISPLAY '*                                     *'.

           MOVE ACU-LIDOS-SEGMEANT     TO WRK-MASCARA.

           DISPLAY '*    LIDOS    SEGMEANT  : ' WRK-MASCARA  ' *'.
           DISPLAY '*                                     *'.

           MOVE ACU-GRAVA-SEGMOVTO     TO WRK-MASCARA.

           DISPLAY '*    GRAVADOS SEGMOVTO  : ' WRK-MASCARA  ' *'.
           DISPLAY '*                                     *'.

           MOVE ACU-GRAVA-RELATO       TO WRK-MASCARA.

           DISPLAY '*    IMPRESSOS          : ' WRK-MASCARA  ' *'.
           DISPLAY '*                                     *'.
           DISPLAY '************** GFCT6855 ***************'.


      *----------------------------------------------------------------*
       4100-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-ROTINA-ERRO               SECTION.
      *----------------------------------------------------------------*

           MOVE 'GFCT6855'             TO ERR-PGM.

           CALL 'POOL7100'             USING WRK-BATCH
                                             ERRO-AREA.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                   EXIT.
      *----------------------------------------------------------------*
