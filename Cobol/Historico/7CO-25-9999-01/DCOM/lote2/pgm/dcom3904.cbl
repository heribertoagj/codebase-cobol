      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM3904.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3904                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: MARCELINO - ALTRAN.                          *
      *     DATA........: 09/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA PARA SELECAO - SELECAO DE SUBPRODUTOS. *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9DTM....: LISTA PARA SELECAO - SELECAO DE SUBPRODUTOS. *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *

      *     I#DCOMSY - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     DCOM4319 - OBTER LISTA SUBPRODUTOS                         *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0103                                     *
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
       77  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3904 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WRK-IND                     PIC S9(09) COMP     VALUE ZEROS.
       77  WIND-SEL                    PIC S9(09) COMP     VALUE ZEROS.
       77  WCT-SELEC                   PIC S9(09) COMP     VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ATRIBUTOS ***'.

      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                     PIC S9(08) COMP     VALUE +225.
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-241                     PIC S9(08) COMP     VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS. CURSOR    ---*
       01  WRK-49361                   PIC S9(08) COMP     VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS. CURSOR    ---*
       01  WRK-49369                   PIC S9(08) COMP     VALUE +49369.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(08)          VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(01)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO GENERICA ***'.

      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP    VALUE +293.
           05  COMU-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)         VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.
               10  COMU-CNPJCPF        PIC  X(017)         VALUE SPACES.
               10  COMU-COD-PROD-X.
                   15  COMU-COD-PROD   PIC  9(003)         VALUE ZEROS.
               10  COMU-COD-SUBP-X.
                   15  COMU-COD-SUBP   PIC  9(003)         VALUE ZEROS.
               10  COMU-DES-SUBP       PIC  X(040)         VALUE SPACES.
               10  COMU-MENSA          PIC  X(079)         VALUE SPACES.


      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(04) COMP     VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE-PFK.
               10  COMU-TRANSACAO-PFK  PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  COMU-PFK-PFK        PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(68)          VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.

           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(07)          VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(07).
           05  WRK-COD-DEPTO           PIC  9(06)          VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.

           05  FILLER                  PIC  X(45)          VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(12)          VALUE
               '- RET.COD ='.
           05  WRK-RETURN-CODE         PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(11)          VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(02)          VALUE ZEROS.
           05  FILLER                  PIC  X(02)          VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DO MODULO DCOM4319 ***'.
      *----------------------------------------------------------------*
       01  WRK-4319E-ENTRADA.
           03  WRK-4319E-COD-RETORNO          PIC  X(004).
           03  WRK-4319E-MSG-RETORNO          PIC  X(079).
           03  WRK-4319E-RESTART              PIC  9(005).
           03  WRK-4319E-FLAG                 PIC  X(001).
           03  WRK-4319E-START-COUNT          PIC  X(001).
           03  WRK-4319E-CFUNC-BDSCO          PIC  9(009).

           03  WRK-4319E-CTERM                PIC  X(008).
           03  WRK-4319E-CPRODT               PIC  9(003).
4S2511*    03  WRK-4319E-CCNPJ-CPF            PIC  9(009).
           03  WRK-4319E-CCNPJ-CPF            PIC  X(09).
4S2511*    03  WRK-4319E-CFLIAL-CNPJ          PIC  9(005).
           03  WRK-4319E-CFLIAL-CNPJ          PIC  X(04).
           03  WRK-4319E-CCTRL-CNPJ-CPF       PIC  9(002).

       01  WRK-4319S-SAIDA.
           03  WRK-4319S-HEADER.
               05  WRK-4319S-COD-RETORNO      PIC  X(004).
               05  WRK-4319S-MSG-RETORNO      PIC  X(079).
               05  WRK-4319S-RESTART          PIC  9(005).
               05  WRK-4319S-FLAG             PIC  X(001).
               05  WRK-4319S-QTDE-RETORNADA   PIC  9(003).
               05  WRK-4319S-COUNT            PIC  9(009).

           03  WRK-4319S-DADOS.
               05  WRK-4319S-TABELA-SAIDA     OCCURS 30  TIMES.
                   07  WRK-4319S-CSPROD-DESC-COML    PIC  9(03).
                   07  WRK-4319S-ISPROD-DESC-COML    PIC  X(40).
                   07  WRK-4319S-IRSUMO-SPROD-DESC   PIC  X(10).
                   07  WRK-4319S-CTPO-DESC-COML      PIC  9(03).

                   07  WRK-4319S-IRSUMO-TPO-DESC     PIC  X(10).
                   07  WRK-4319S-CINDCD-CONTR-LIM    PIC  X(01).
                   07  WRK-4319S-CINDCD-CONVE-CLI    PIC  X(01).
                   07  WRK-4319S-CINDCD-CONVE-GRAL   PIC  X(01).
                   07  WRK-4319S-CRESP-LIM-CREDT     PIC  X(01).
                   07  WRK-4319S-CINDCD-CONS-CREDT   PIC  X(01).
                   07  WRK-4319S-CINDCD-ANLSE-CREDT  PIC  X(01).
                   07  WRK-4319S-CINDCD-NEGOC-DIFER  PIC  X(01).

           03  WRK-4319S-CONSISTENCIA REDEFINES WRK-4319S-DADOS.
               05  WRK-4319S-TAMANHO-ERRO     PIC  9(001).
               05  WRK-4319S-TABELA-ERROS     OCCURS  1 TIMES.
                   07  WRK-4319S-CAMPOS-ERRO  PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9DTM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-9DTM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-9DTM        PIC  X(01)          VALUE SPACES.

           05  WRK-INP-SENHAS-9DTM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-9DTM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-9DTM.
               10  WRK-INP-TIPOOPC-9DTM
                                       PIC  X(01)          VALUE SPACES.
               10  WRK-INP-TMSTAMP-9DTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-CHAMADO-9DTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-PROD-9DTM-X.
                   15  WRK-INP-PROD-9DTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-CNPJCPF-9DTM
                                       PIC  X(17)          VALUE SPACES.
               10  WRK-INP-ULTPAG-9DTM PIC  X(01)          VALUE SPACE.
               10  WRK-INP-PAGINA-9DTM-X.
                   15  WRK-INP-PAGINA-9DTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-LINHAS-9DTM OCCURS 10 TIMES.
                   15  WRK-INP-SELEC-9DTM
                                       PIC  X(01)          VALUE SPACES.

                   15  WRK-INP-COD-9DTM-X.
                       20  WRK-INP-COD-9DTM
                                       PIC  9(03)          VALUE ZEROS.
                   15  WRK-INP-DESC-9DTM
                                       PIC  X(40)          VALUE SPACES.
                   15  WRK-INP-SIT-9DTM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-INP-MENSA-9DTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOM9DTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-9DTM.
           05  WRK-OUT-LL-9DTM         PIC S9(04) COMP     VALUE +0807.
           05  WRK-OUT-ZZ-9DTM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-9DTM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-9DTM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-9DTM.
               10  WRK-OUT-TIPOOPC-9DTM
                                       PIC  X(01)          VALUE SPACES.

               10  WRK-OUT-TMSTAMP-9DTM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-9DTM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-PROD-9DTM-X.
                   15  WRK-OUT-PROD-9DTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-CNPJCPF-9DTM
                                       PIC  X(17)          VALUE SPACES.
               10  WRK-OUT-ULTPAG-9DTM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PAGINA-9DTM-X.
                   15  WRK-OUT-PAGINA-9DTM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-LINHAS-9DTM OCCURS 10 TIMES.
                   15  WRK-OUT-SELEC-ATTR-9DTM
                                       PIC S9(004) COMP    VALUE ZEROS.
                   15  WRK-OUT-SELEC-9DTM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-OUT-COD-9DTM-X.
                       20  WRK-OUT-COD-9DTM
                                       PIC  9(03)          VALUE ZEROS.

                   15  WRK-OUT-DESC-9DTM
                                       PIC  X(40)          VALUE SPACES.
                   15  WRK-OUT-SIT-9DTM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-OUT-MENSA-9DTM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9DTM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOM9DTM.
           05  WRK-660-LL-AREA-9DTM    PIC  9(004) COMP    VALUE 0104.
           05  WRK-660-LL-MENSA-9DTM   PIC  9(004) COMP    VALUE 0807.
           05  WRK-660-SENHAS-9DTM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-9DTM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TIPOOPC-9DTM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-TMSTAMP-9DTM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-9DTM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-PROD-9DTM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-CNPJCPF-9DTM    PIC  9(004) COMP    VALUE 0017.
           05  WRK-660-ULTPAG-9DTM     PIC  9(004) COMP    VALUE 0001.

           05  WRK-660-PAGINA-9DTM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-600-LISTA           OCCURS  10.
               10 WRK-660-ESCOLH-9DTM  PIC  9(004) COMP    VALUE 2003.
               10 WRK-660-COD-9DTM     PIC  9(004) COMP    VALUE 0003.
               10 WRK-660-DESC-9DTM    PIC  9(004) COMP    VALUE 0040.
               10 WRK-660-SIT-9DTM     PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-9DTM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*
       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*
       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*

       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3904 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-IO-STATUS            PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-IO-MODNAME           PIC  X(08).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(08).
           05 FILLER                   PIC  X(02).
           05 LNK-ALT-STATUS           PIC  X(02).
           05 FILLER                   PIC  X(12).
           05 LNK-ALT-MODNAME          PIC  X(08).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *     PROCEDIMENTOS INICIAIS                                     *
      *----------------------------------------------------------------*
       0000-INICIAR                    SECTION.
      *----------------------------------------------------------------*

           CALL 'POOL5005'             USING WRK-VERSAO
                                             WRK-FUNCAO
                                             LNK-IO-PCB
                                             LNK-ALT-PCB
                                             WRK-MENSAGEM

                                             WRK-TELA
                                             WRK-COD-USER
                                             WRK-COD-DEPTO.

           IF  RETURN-CODE             EQUAL 04 OR 12
               PERFORM 3000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO WRK-OUT-DADOS-9DTM
                                          WRK-OUT-SENHAS-9DTM
                                          WRK-OUT-COMANDO-9DTM
                                          COMU-DADOS.

           PERFORM 2000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     PROCESSAR MENSAGEM                                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOM9DTM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3904'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOM9DTM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOM9DTM'
               PERFORM 2300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     RECEBER CONTROLE                                           *
      *----------------------------------------------------------------*
       2100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO COMU-AREA
           MOVE WRK-TRANSACAO          TO WRK-OUT-CHAMADO-9DTM.
           MOVE COMU-OPCAO             TO WRK-OUT-TIPOOPC-9DTM.
           MOVE COMU-TIMESTAMP         TO WRK-OUT-TMSTAMP-9DTM.
           MOVE COMU-COD-PROD          TO WRK-OUT-PROD-9DTM.
           MOVE COMU-CNPJCPF           TO WRK-OUT-CNPJCPF-9DTM.
           MOVE 1                      TO WRK-OUT-PAGINA-9DTM.

           MOVE 'N'                    TO WRK-4319E-START-COUNT
           MOVE 'N'                    TO WRK-4319E-FLAG
           MOVE ZEROS                  TO WRK-4319E-RESTART
           MOVE WRK-COD-USER           TO WRK-4319E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4319E-CTERM
           MOVE COMU-COD-PROD          TO WRK-4319E-CPRODT

4S2511*    MOVE COMU-CNPJCPF(1:9)      TO WRK-4319E-CCNPJ-CPF
4S2511*    MOVE COMU-CNPJCPF(11:4)     TO WRK-4319E-CFLIAL-CNPJ
4S2511*    MOVE COMU-CNPJCPF(16:2)     TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     IF COMU-CNPJCPF          EQUAL SPACES OR
4S2511        COMU-CNPJCPF          EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-4319E-CCNPJ-CPF
4S2511         MOVE SPACES          TO WRK-4319E-CFLIAL-CNPJ
4S2511         MOVE ZEROS           TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     ELSE
4S2511         MOVE COMU-CNPJCPF(1:9)   TO WRK-4319E-CCNPJ-CPF
4S2511         MOVE COMU-CNPJCPF(11:4)  TO WRK-4319E-CFLIAL-CNPJ
4S2511         MOVE COMU-CNPJCPF(16:2)  TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     END-IF
           MOVE 'DCOM4319'             TO WRK-MODULO

           CALL WRK-MODULO             USING WRK-4319E-ENTRADA
                                             WRK-4319S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4319S-COD-RETORNO
              WHEN '0000'
                   IF (WRK-4319S-CSPROD-DESC-COML(11)  NOT NUMERIC) OR
                      (WRK-4319S-CSPROD-DESC-COML(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9DTM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9DTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9DTM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9DTM

                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   MOVE WRK-TRANSACAO          TO WRK-TELA
                   MOVE ZEROS                  TO COMU-COD-SUBP
                   MOVE SPACES                 TO COMU-DES-SUBP
                   MOVE WRK-4319S-MSG-RETORNO  TO COMU-MENSA
                   MOVE WRK-CHNG               TO WRK-FUNCAO
                   MOVE 'DCOM3904'             TO COMU-TRANSACAO
                   MOVE COMU-AREA              TO WRK-MENSAGEM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (WRK-4319S-CSPROD-DESC-COML(WRK-IND)  NOT NUMERIC) OR
              (WRK-4319S-CSPROD-DESC-COML(WRK-IND)  EQUAL ZEROS)
               MOVE WRK-225          TO WRK-OUT-SELEC-ATTR-9DTM(WRK-IND)
           ELSE
               MOVE WRK-4319S-CSPROD-DESC-COML(WRK-IND)
                                     TO WRK-OUT-COD-9DTM(WRK-IND)
               MOVE WRK-4319S-ISPROD-DESC-COML(WRK-IND)
                                     TO WRK-OUT-DESC-9DTM(WRK-IND)
               MOVE WRK-4319S-IRSUMO-SPROD-DESC(WRK-IND)
                                     TO WRK-OUT-SIT-9DTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOM9DTM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOM9DTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-9DTM

           PERFORM 2205-DEVOLVER-TELA-FIXO

           EVALUATE TRUE
               WHEN WRK-INP-PFK-9DTM      EQUAL 'H' OR
                   (WRK-INP-PFK-9DTM      EQUAL '.' AND
                    WRK-INP-COMANDO-9DTM  EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-9DTM TO  WRK-OUT-MENSA-9DTM

               WHEN WRK-INP-PFK-9DTM     EQUAL '3' OR
                   (WRK-INP-PFK-9DTM     EQUAL '.' AND

                    WRK-INP-COMANDO-9DTM EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-9DTM     EQUAL '7' OR
                   (WRK-INP-PFK-9DTM     EQUAL '.' AND
                    WRK-INP-COMANDO-9DTM EQUAL 'PFK07')
                    PERFORM 2240-TELA-ANTERIOR

               WHEN WRK-INP-PFK-9DTM     EQUAL '8' OR
                   (WRK-INP-PFK-9DTM     EQUAL '.' AND
                    WRK-INP-COMANDO-9DTM EQUAL 'PFK08')
                    PERFORM 2250-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-9DTM          EQUAL '.' AND
                     WRK-INP-COMANDO-9DTM      EQUAL SPACES)
                     PERFORM 2270-TRATAR-ENTER
                     PERFORM 2210-DEVOLVER-TELA-VARIA

               WHEN  OTHER
                     PERFORM 2210-DEVOLVER-TELA-VARIA
                     MOVE SG-MENSAGEM(38)

                                       TO  WRK-OUT-MENSA-9DTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-SENHAS-9DTM    TO WRK-OUT-SENHAS-9DTM
           MOVE WRK-INP-TIPOOPC-9DTM   TO WRK-OUT-TIPOOPC-9DTM
           MOVE WRK-INP-TMSTAMP-9DTM   TO WRK-OUT-TMSTAMP-9DTM
           MOVE WRK-INP-ULTPAG-9DTM    TO WRK-OUT-ULTPAG-9DTM
           MOVE WRK-INP-PAGINA-9DTM-X  TO WRK-OUT-PAGINA-9DTM-X
           MOVE WRK-INP-CHAMADO-9DTM   TO WRK-OUT-CHAMADO-9DTM
           MOVE WRK-INP-PROD-9DTM-X    TO WRK-OUT-PROD-9DTM-X

           MOVE WRK-INP-CNPJCPF-9DTM   TO WRK-OUT-CNPJCPF-9DTM
           MOVE WRK-INP-MENSA-9DTM     TO WRK-OUT-MENSA-9DTM.

      *----------------------------------------------------------------*
       2205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS VARIAVEIS                            *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM  VARYING  WRK-IND  FROM 1 BY 1
                      UNTIL  WRK-IND  GREATER 10

               IF (WRK-INP-COD-9DTM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-COD-9DTM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-9DTM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-9DTM(WRK-IND)

                                     TO WRK-OUT-SELEC-9DTM(WRK-IND)
                   MOVE WRK-INP-COD-9DTM-X(WRK-IND)
                                     TO WRK-OUT-COD-9DTM-X(WRK-IND)
                   MOVE WRK-INP-DESC-9DTM(WRK-IND)
                                     TO WRK-OUT-DESC-9DTM(WRK-IND)
                   MOVE WRK-INP-SIT-9DTM(WRK-IND)
                                     TO WRK-OUT-SIT-9DTM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0103                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA

           MOVE SPACES                 TO COMU-DADOS

           MOVE LENGTH OF COMU-AREA    TO COMU-LL
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE WRK-INP-CHAMADO-9DTM   TO WRK-TELA
           MOVE 'DCOM3904'             TO COMU-TRANSACAO
           MOVE '3'                    TO COMU-PFK
           MOVE WRK-INP-SENHAS-9DTM    TO COMU-SENHAS
           MOVE WRK-INP-COMANDO-9DTM   TO COMU-COMANDO
           MOVE WRK-INP-TIPOOPC-9DTM   TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-9DTM   TO COMU-TIMESTAMP
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2240-TELA-ANTERIOR              SECTION.

      *----------------------------------------------------------------*

           IF  WRK-INP-PAGINA-9DTM  EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MENSA-9DTM
               GO TO 2240-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-4319E-START-COUNT
           MOVE 'N'                    TO WRK-4319E-FLAG
           MOVE WRK-COD-USER           TO WRK-4319E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4319E-CTERM
           MOVE WRK-INP-PROD-9DTM      TO WRK-4319E-CPRODT
4S2511*    MOVE WRK-INP-CNPJCPF-9DTM(1:9)  TO WRK-4319E-CCNPJ-CPF
4S2511*    MOVE WRK-INP-CNPJCPF-9DTM(11:4) TO WRK-4319E-CFLIAL-CNPJ
4S2511*    MOVE WRK-INP-CNPJCPF-9DTM(16:2) TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     IF WRK-INP-CNPJCPF-9DTM  EQUAL SPACES OR
4S2511        WRK-INP-CNPJCPF-9DTM  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-4319E-CCNPJ-CPF
4S2511         MOVE SPACES          TO WRK-4319E-CFLIAL-CNPJ
4S2511         MOVE ZEROS           TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     ELSE
4S2511         MOVE WRK-INP-CNPJCPF-9DTM(1:9)     
                                    TO WRK-4319E-CCNPJ-CPF
4S2511         MOVE WRK-INP-CNPJCPF-9DTM(11:4)  
                                    TO WRK-4319E-CFLIAL-CNPJ
4S2511         MOVE WRK-INP-CNPJCPF-9DTM(16:2)  
                                    TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     END-IF
           COMPUTE WRK-4319E-RESTART = (WRK-INP-PAGINA-9DTM - 2) * 10

           MOVE 'DCOM4319'             TO WRK-MODULO
           CALL WRK-MODULO             USING WRK-4319E-ENTRADA
                                             WRK-4319S-SAIDA

                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4319S-COD-RETORNO
              WHEN '0000'
                   IF (WRK-4319S-CSPROD-DESC-COML(11)  NOT NUMERIC) OR
                      (WRK-4319S-CSPROD-DESC-COML(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9DTM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9DTM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9DTM
                       MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9DTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO


              WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4319S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9DTM
           END-EVALUATE.


           COMPUTE WRK-OUT-PAGINA-9DTM = WRK-INP-PAGINA-9DTM - 1.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2250-TELA-POSTERIOR             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-9DTM     EQUAL '*'

               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MENSA-9DTM
               GO TO 2250-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-4319E-START-COUNT
           MOVE 'N'                    TO WRK-4319E-FLAG
           MOVE WRK-COD-USER           TO WRK-4319E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4319E-CTERM
           MOVE WRK-INP-PROD-9DTM      TO WRK-4319E-CPRODT
4S2511*    MOVE WRK-INP-CNPJCPF-9DTM(1:9)  TO WRK-4319E-CCNPJ-CPF
4S2511*    MOVE WRK-INP-CNPJCPF-9DTM(11:4) TO WRK-4319E-CFLIAL-CNPJ
4S2511*    MOVE WRK-INP-CNPJCPF-9DTM(16:2) TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     IF WRK-INP-CNPJCPF-9DTM  EQUAL SPACES OR
4S2511        WRK-INP-CNPJCPF-9DTM  EQUAL LOW-VALUES
4S2511         MOVE SPACES          TO WRK-4319E-CCNPJ-CPF
4S2511         MOVE SPACES          TO WRK-4319E-CFLIAL-CNPJ
4S2511         MOVE ZEROS           TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     ELSE
4S2511         MOVE WRK-INP-CNPJCPF-9DTM(1:9)   
                                    TO WRK-4319E-CCNPJ-CPF
4S2511         MOVE WRK-INP-CNPJCPF-9DTM(11:4)  
                                    TO WRK-4319E-CFLIAL-CNPJ
4S2511         MOVE WRK-INP-CNPJCPF-9DTM(16:2) 
                                    TO WRK-4319E-CCTRL-CNPJ-CPF
4S2511     END-IF
           COMPUTE WRK-4319E-RESTART = (WRK-INP-PAGINA-9DTM) * 10

           MOVE 'DCOM4319'             TO WRK-MODULO
           CALL WRK-MODULO             USING WRK-4319E-ENTRADA
                                             WRK-4319S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA


           EVALUATE WRK-4319S-COD-RETORNO
              WHEN '0000'
                   IF (WRK-4319S-CSPROD-DESC-COML(11)  NOT NUMERIC) OR
                      (WRK-4319S-CSPROD-DESC-COML(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9DTM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9DTM
                   ELSE
                      MOVE SPACES      TO WRK-OUT-ULTPAG-9DTM
                      MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9DTM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

              WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

              WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA

                   MOVE WRK-4319S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9DTM
           END-EVALUATE.


           COMPUTE WRK-OUT-PAGINA-9DTM = WRK-INP-PAGINA-9DTM + 1.

      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE  SPACES                TO WRK-INCONSIS

           PERFORM 2271-CONSISTIR-FISICA

           IF  WRK-INCONSIS            EQUAL  '*'
               GO TO 2270-99-FIM

           END-IF.

           PERFORM 2272-TRANSFERIR-CONTROLE-0103.

      *----------------------------------------------------------------*
       2270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2271-CONSISTIR-FISICA           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WCT-SELEC

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 10

               IF  WRK-INP-SELEC-9DTM(WRK-IND) NOT EQUAL SPACE AND 'X'
                   MOVE '*'          TO WRK-INCONSIS
                   MOVE WRK-49369    TO WRK-OUT-SELEC-ATTR-9DTM(WRK-IND)
                   MOVE SG-MENSAGEM(41)
                                     TO WRK-OUT-MENSA-9DTM

               ELSE
                   IF  WRK-INP-SELEC-9DTM(WRK-IND) EQUAL 'X'
                       MOVE WRK-49369
                                     TO WRK-OUT-SELEC-ATTR-9DTM(WRK-IND)
                       ADD 1         TO WCT-SELEC
                       MOVE WRK-IND  TO WIND-SEL
                   END-IF
               END-IF
           END-PERFORM.

           IF WRK-INCONSIS  EQUAL  '*'
              GO TO 2271-99-FIM
           END-IF.

           IF  WCT-SELEC               EQUAL 0  OR  GREATER 1
               MOVE '*'                TO WRK-INCONSIS
               IF  WCT-SELEC           EQUAL 0
                   MOVE SG-MENSAGEM(53)
                                       TO WRK-OUT-MENSA-9DTM
               ELSE
                 IF  WCT-SELEC         GREATER 1

                     MOVE SG-MENSAGEM(39)
                                       TO WRK-OUT-MENSA-9DTM
                 END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE-0103   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA
           MOVE WRK-CHNG               TO WRK-FUNCAO

           MOVE LENGTH OF COMU-AREA    TO COMU-LL
           MOVE WRK-INP-CHAMADO-9DTM   TO WRK-TELA
           MOVE WRK-INP-TIPOOPC-9DTM   TO COMU-OPCAO
           MOVE WRK-INP-TMSTAMP-9DTM   TO COMU-TIMESTAMP

           MOVE ZEROS                  TO COMU-COD-PROD
           MOVE WRK-INP-COD-9DTM(WIND-SEL)
                                       TO COMU-COD-SUBP
           MOVE WRK-INP-DESC-9DTM(WIND-SEL)
                                       TO COMU-DES-SUBP

           MOVE 'DCOM3904'             TO COMU-TRANSACAO
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-9DTM
                                             WRK-660-DCOM9DTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS

               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-9DTM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*

       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              NOT EQUAL SPACES AND LOW-VALUES
               MOVE 'DCOM3904'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3904'         TO ERR-MODULO
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO

           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB

                                             WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING LNK-IO-PCB
                                             ERRO-AREA
                                             LNK-ALT-PCB
           END-IF.

           PERFORM 3000-FINALIZAR.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
