      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID.     DCOM3909.
       AUTHOR.         FABRICA - ALTRAN.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM3909                                     *
      *     PROGRAMADOR.: FABRICA - ALTRAN.                            *
      *     ANALISTA....: MARCELINO - ALTRAN.                          *
      *     DATA........: 06/01/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: LISTA PARA SELECAO                           *
      *                   CONTRATO LIMITE                              *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOM9ITM....: LISTA PARA SELECAO                           *
      *                   CONTRATO LIMITE                              *
      *----------------------------------------------------------------*

      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM3906                                     *
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
       01  FILLER                      PIC  X(50)          VALUE
           '*** DCOM3909 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
       77  WRK-IND                     PIC  9(09)          VALUE ZEROS.
       77  WIND-SEL                    PIC  9(09)          VALUE ZEROS.
       77  WCT-SELEC                   PIC  9(09)          VALUE ZEROS.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE

           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALFANUME - NORMAL    - PROTEGIDO    - NAO POS CURSOR ---*
       01  WRK-225                     PIC S9(08) COMP     VALUE +225.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(08) COMP     VALUE +49353.
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
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.

           05  WRK-MSG-DCOM7995.

               10  FILLER              PIC  X(36)          VALUE
                   'ERRO NO ACESSO AO MODULO DCOM7995 - '.
               10  WRK-TEXTO           PIC  X(43)          VALUE SPACES.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(50)      VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(04)          VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)          VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)          VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)          VALUE SPACES.
           05  WRK-TELA                PIC  X(08)          VALUE SPACES.
           05  WRK-VERSAO              PIC  X(06)          VALUE
                                                           'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).

           05  WRK-COD-DEPTO.

               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-COD-AGENC-N         PIC  9(005)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)          VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DA TELA DCOM9ITM - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-9ITM.
           05  FILLER                  PIC  X(18)          VALUE SPACES.
           05  WRK-INP-PFK-9ITM        PIC  X(01)          VALUE SPACES.
           05  WRK-INP-SENHAS-9ITM     PIC  X(37)          VALUE SPACES.
           05  WRK-INP-COMANDO-9ITM    PIC  X(68)          VALUE SPACES.
           05  WRK-INP-DADOS-9ITM.
               10  WRK-INP-TMSTAMP-9ITM

                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-CHAMADO-9ITM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-INP-TPOPCAO-9ITM
                                       PIC  X(01)          VALUE SPACE.
               10  WRK-INP-ULTPAG-9ITM PIC  X(01)          VALUE SPACE.
               10  WRK-INP-PROD-9ITM-X.
                   15  WRK-INP-PROD-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-SUBP-9ITM-X.
                   15  WRK-INP-SUBP-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-PAGINA-9ITM-X.
                   15  WRK-INP-PAGINA-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-NOMECL-9ITM PIC  X(30)          VALUE SPACES.
               10  WRK-INP-CGCCPF-9ITM.
                   15  WRK-INP-CGCCPF-N PIC  X(11)         VALUE ZEROS.
                   15  FILLER           PIC  X(01)         VALUE '/'.
4S2511*            15  WRK-INP-CONT-N   PIC  9(04)         VALUE ZEROS.
4S2511             15  WRK-INP-CONT-N   PIC  X(04)         VALUE SPACES.
                   15  FILLER           PIC  X(01)         VALUE '/'.

                   15  WRK-INP-FILIAL-N PIC  9(02)         VALUE ZEROS.
               10  WRK-INP-BANCO-9ITM-X.
                   15  WRK-INP-BANCO-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-INP-AGENCIA-9ITM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-INP-CONTA-9ITM-X.
                   15  WRK-INP-CONTA-9ITM
                                       PIC  X(13)          VALUE ZEROS.
               10  WRK-INP-LINHAS-9ITM OCCURS 10 TIMES.
                   15  WRK-INP-SELEC-9ITM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-INP-NUMERO-9ITM-X.
                       20  WRK-INP-NUMERO-9ITM
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-INP-VLRLIM-9ITM-X.
                       20  WRK-INP-VLRLIM-9ITM
                                       PIC  X(14)          VALUE SPACES.
                   15  WRK-INP-VLRDIS-9ITM-X.
                       20  WRK-INP-VLRDIS-9ITM
                                       PIC  X(14)          VALUE SPACES.

                   15  WRK-INP-DTINIC-9ITM
                                       PIC  X(10)          VALUE SPACES.
                   15  WRK-INP-DTVENC-9ITM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-INP-MENSA-9ITM      PIC  X(79)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                          PIC  X(50)      VALUE
           '*** AREA DA TELA DCOM9ITM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-9ITM.
           05  WRK-OUT-LL-9ITM         PIC S9(04) COMP     VALUE +0000.
           05  WRK-OUT-ZZ-9ITM         PIC S9(04) COMP     VALUE ZEROS.
           05  WRK-OUT-SENHAS-9ITM     PIC  X(37)          VALUE SPACES.
           05  WRK-OUT-COMANDO-9ITM    PIC  X(68)          VALUE SPACES.
           05  WRK-OUT-DADOS-9ITM.
               10  WRK-OUT-TMSTAMP-9ITM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CHAMADO-9ITM
                                       PIC  X(08)          VALUE SPACES.
               10  WRK-OUT-TPOPCAO-9ITM

                                       PIC  X(01)          VALUE SPACES.
               10  WRK-OUT-ULTPAG-9ITM PIC  X(01)          VALUE SPACE.
               10  WRK-OUT-PROD-9ITM-X.
                   15  WRK-OUT-PROD-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-SUBP-9ITM-X.
                   15  WRK-OUT-SUBP-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-PAGINA-9ITM-X.
                   15  WRK-OUT-PAGINA-9ITM
                                       PIC  9(03)          VALUE ZEROS.
               10  WRK-OUT-NOMECL-9ITM PIC  X(30)          VALUE SPACES.
               10  WRK-OUT-CGCCPF-9ITM.
4S2511*        15  WRK-OUT-CGCCPF-N PIC  999.999.999   VALUE ZEROS.
                   15  WRK-OUT-CGCCPF-N PIC  X(11)         VALUE SPACES.
                   15  FILLER           PIC  X(01)         VALUE '/'.
                   15  WRK-OUT-CONT-N   PIC  9(04)         VALUE ZEROS.
                   15  FILLER           PIC  X(01)         VALUE '/'.
                   15  WRK-OUT-FILIAL-N PIC  9(02)         VALUE ZEROS.
               10  WRK-OUT-BANCO-9ITM-X.
                   15  WRK-OUT-BANCO-9ITM
                                       PIC  9(03)          VALUE ZEROS.

               10  WRK-OUT-AGENCIA-9ITM
                                       PIC  X(26)          VALUE SPACES.
               10  WRK-OUT-CONTA-9ITM-X.
                   15  WRK-OUT-CONTA-9ITM
                                       PIC  9(13)          VALUE ZEROS.
               10  WRK-OUT-LINHAS-9ITM OCCURS 10 TIMES.
                   15  WRK-OUT-SELEC-ATTR-9ITM
                                       PIC S9(004) COMP    VALUE ZEROS.
                   15  WRK-OUT-SELEC-9ITM
                                       PIC  X(01)          VALUE SPACES.
                   15  WRK-OUT-NUMERO-9ITM-X.
                       20  WRK-OUT-NUMERO-9ITM
                                       PIC  9(09)          VALUE ZEROS.
                   15  WRK-OUT-VLRLIM-9ITM-X.
                       20  WRK-OUT-VLRLIM-9ITM
                                       PIC  ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-VLRDIS-9ITM-X.
                       20  WRK-OUT-VLRDIS-9ITM
                                       PIC  ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-DTINIC-9ITM
                                       PIC  X(10)          VALUE SPACES.

                   15  WRK-OUT-DTVENC-9ITM
                                       PIC  X(10)          VALUE SPACES.
           05  WRK-OUT-MENSA-9ITM      PIC  X(79)          VALUE SPACES.

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

           '*** AREA DA TELA DCOM9ITM - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOM9ITM.
           05  WRK-660-LL-AREA-9ITM    PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-LL-MENSA-9ITM   PIC  9(004) COMP    VALUE 0000.
           05  WRK-660-SENHAS-9ITM     PIC  9(004) COMP    VALUE 0037.
           05  WRK-660-COMANDO-9ITM    PIC  9(004) COMP    VALUE 0068.
           05  WRK-660-TMSTAMP-9ITM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-CHAMADO-9ITM    PIC  9(004) COMP    VALUE 0008.
           05  WRK-660-TIPOOPC-9ITM    PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-ULTPAG-9ITM     PIC  9(004) COMP    VALUE 0001.
           05  WRK-660-PROD-9ITM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-SUBP-9ITM       PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-PAGINA-9ITM     PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-NOMECL-9ITM     PIC  9(004) COMP    VALUE 0030.
           05  WRK-660-CGCCPF-9ITM     PIC  9(004) COMP    VALUE 0019.
           05  WRK-660-BANCO-9ITM      PIC  9(004) COMP    VALUE 0003.
           05  WRK-660-AGENCIA-9ITM    PIC  9(004) COMP    VALUE 0026.
           05  WRK-660-NUCONTA-9ITM    PIC  9(004) COMP    VALUE 0013.
           05  WRK-600-LISTA-9ITM      OCCURS  10.
               10 WRK-660-ESCOLH-9ITM  PIC  9(004) COMP    VALUE 2003.

               10 WRK-660-NUMERO-9ITM  PIC  9(004) COMP    VALUE 0009.
               10 WRK-660-VLRLIM-9ITM  PIC  9(004) COMP    VALUE 0014.
               10 WRK-660-VLRDIS-9ITM  PIC  9(004) COMP    VALUE 0014.
               10 WRK-660-DTINIC-9ITM  PIC  9(004) COMP    VALUE 0010.
               10 WRK-660-DTVENC-9ITM  PIC  9(004) COMP    VALUE 0010.
           05  WRK-660-MENSA-9ITM      PIC  9(004) COMP    VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM0103 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-103.
           05  COMU-LL-103             PIC S9(04) COMP     VALUE +354.
           05  COMU-ZZ-103             PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE-103.
               10  COMU-TRANSACAO-103  PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  COMU-PFK-103        PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS-103         PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO-103        PIC  X(68)          VALUE SPACES.
           05  COMU-DADOS-103.

               10  COMU-TPOPCAO-103    PIC  9(01)          VALUE ZERO.
               10  COMU-TIMESTAMP-103  PIC  X(26)          VALUE SPACES.
               10  COMU-CHAMADO-103    PIC  X(08)          VALUE SPACES.
               10  COMU-NOME-103       PIC  X(40)          VALUE SPACES.
               10  COMU-CPFCNPJ-103    PIC  X(17)          VALUE SPACES.
               10  COMU-BANCO-103.
                   15  COMU-BANCO-103-N    PIC 9(03)       VALUE ZEROS.
               10  COMU-AGENCIA-103        PIC X(26)       VALUE SPACES.
               10  COMU-CONTA-103.
                   15  COMU-CONTA-103-N    PIC 9(13)       VALUE ZEROS.
               10  COMU-PROD-103.
                   15  COMU-PROD-103-N     PIC 9(03)       VALUE ZEROS.
               10  COMU-SUBP-103.
                   15  COMU-SUBP-103-N     PIC 9(03)       VALUE ZEROS.
               10  COMU-COD-OPER-103.
                   15  COMU-COD-OPER-103-N PIC 9(09)       VALUE ZEROS.
               10  COMU-TP-OPER-103        PIC X(02)       VALUE SPACES.
               10  COMU-MENSA-103          PIC X(79)       VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE

           '*** AREA DE COMUNICACAO PFK ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(04) COMP     VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(04) COMP     VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(08)          VALUE SPACES.
               10  FILLER              PIC  X(06)          VALUE SPACES.
               10  COMU-PFK-PFK        PIC  X(01)          VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(37)          VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(68)          VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(50)          VALUE
           '*** AREA DE COMUNICACAO DCOM4116 ***'.
      *----------------------------------------------------------------*
       01  WRK-4116E-ENTRADA.
           03  WRK-4116E-HEADER.
               05  WRK-4116E-COD-RETORNO   PIC  X(04).
               05  WRK-4116E-MSG-RETORNO   PIC  X(79).
               05  WRK-4116E-RESTART       PIC  9(05).

               05  WRK-4116E-FLAG          PIC  X(01).
               05  WRK-4116E-START-COUNT   PIC  X(01).
               05  WRK-4116E-CFUNC-BDSCO   PIC  9(09).
               05  WRK-4116E-CTERM         PIC  X(08).
           03  WRK-4116E-DADOS.
               05  WRK-4116E-CPRODT           PIC  9(003).
               05  WRK-4116E-CSPROD-DESC-COML PIC  9(003).
               05  WRK-4116E-CAG-BCRIA        PIC  9(005).
               05  WRK-4116E-CCTA-BCRIA-CLI   PIC  9(013).
4S2511*        05  WRK-4116E-CCNPJ-CPF        PIC  9(009).
               05  WRK-4116E-CCNPJ-CPF        PIC  X(09).
4S2511*        05  WRK-4116E-CFLIAL-CNPJ      PIC  9(004).
               05  WRK-4116E-CFLIAL-CNPJ      PIC  X(04).
               05  WRK-4116E-CCTRL-CNPJ-CPF   PIC  9(002).

       01  WRK-4116S-SAIDA.
           03  WRK-4116S-HEADER.
               05  WRK-4116S-COD-RETORNO
                                       PIC  X(04).
               05  WRK-4116S-MSG-RETORNO
                                       PIC  X(79).
               05  WRK-4116S-RESTART   PIC  9(05).
               05  WRK-4116S-FLAG      PIC  X(01).

               05  WRK-4116S-QTDE-RETORNADA
                                       PIC  9(03).
               05  WRK-4116S-COUNT     PIC  9(09).

           03  WRK-4116S-DADOS.
               05  WRK-4116S-TABELA-SAIDA  OCCURS 20  TIMES.
                   07  WRK-4116S-CCONTR-LIM-DESC
                                           PIC  9(009).
                   07  WRK-4116S-IABREV-PRODT PIC  X(012).
                   07  WRK-4116S-IPSSOA-DESC-COML
                                           PIC  X(060).
                   07  WRK-4116S-VCONTR-LIM-DESC
                                           PIC  9(015)V99.
                   07  WRK-4116S-VDISPN-CONTR-LIM
                                           PIC  9(015)V99.
                   07  WRK-4116S-DVCTO-CONTR-LIM
                                           PIC  X(010).
                   07  WRK-4116S-DINIC-CONTR-LIM
                                           PIC  X(010).
           03  WRK-4116S-CONSISTENCIA  REDEFINES  WRK-4116S-DADOS.
               05  WRK-4116S-TAMANHO-ERRO

                                       PIC  9(01).
               05  WRK-4116S-TABELA-ERROS  OCCURS 05  TIMES.
                   07  WRK-4116S-CAMPOS-ERRO
                                       PIC  9(01).

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
           '*** DCOM3909 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO WRK-OUT-DADOS-9ITM
                                          WRK-OUT-SENHAS-9ITM
                                          WRK-OUT-COMANDO-9ITM.

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

           MOVE 'DCOM9ITM'             TO WRK-TELA

           IF  WRK-TRANSACAO           NOT EQUAL 'DCOM3909'
               PERFORM 2100-RECEBER-CONTROLE
           ELSE
               PERFORM 2200-PROCESSAR-DCOM9ITM
           END-IF.

           IF  WRK-TELA                EQUAL 'DCOM9ITM'
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

           MOVE WRK-MENSAGEM           TO COMU-AREA-103.
           MOVE COMU-TIMESTAMP-103     TO WRK-OUT-TMSTAMP-9ITM.
           MOVE COMU-CHAMADO-103       TO WRK-OUT-CHAMADO-9ITM.
           MOVE COMU-NOME-103          TO WRK-OUT-NOMECL-9ITM.
           STRING COMU-CPFCNPJ-103(01:3) '.'
                  COMU-CPFCNPJ-103(04:3) '.'
                  COMU-CPFCNPJ-103(07:3) '/'
                  COMU-CPFCNPJ-103(11:4) '-'
                  COMU-CPFCNPJ-103(16:2) DELIMITED BY SIZE
                                     INTO WRK-OUT-CGCCPF-9ITM.
           MOVE COMU-BANCO-103-N       TO WRK-OUT-BANCO-9ITM.
           MOVE COMU-AGENCIA-103       TO WRK-OUT-AGENCIA-9ITM.
           MOVE COMU-CONTA-103-N       TO WRK-OUT-CONTA-9ITM.
           MOVE COMU-PROD-103-N        TO WRK-OUT-PROD-9ITM.
           MOVE COMU-SUBP-103-N        TO WRK-OUT-SUBP-9ITM.
           MOVE 1                      TO WRK-OUT-PAGINA-9ITM.
           MOVE 'N'                    TO WRK-4116E-FLAG
           MOVE 'N'                    TO WRK-4116E-START-COUNT

           MOVE ZEROS                  TO WRK-4116E-RESTART
      *    PARA A CONSULTA DO CONTRATO LIMITE, NAO DEVE-SE VERIFICAR O
      *    PERFIL PELA AGENCIA DO SENHAS
           MOVE COMU-AGENCIA-103(1:5)  TO WRK-COD-AGENC-N
           MOVE WRK-COD-AGENC-N        TO WRK-4116E-MSG-RETORNO(1:5)
           MOVE WRK-COD-USER           TO WRK-4116E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4116E-CTERM
           MOVE COMU-PROD-103-N        TO WRK-4116E-CPRODT
           MOVE COMU-SUBP-103-N        TO WRK-4116E-CSPROD-DESC-COML
           MOVE COMU-AGENCIA-103(1:5)  TO WRK-4116E-CAG-BCRIA.
           MOVE COMU-CONTA-103-N       TO WRK-4116E-CCTA-BCRIA-CLI.
           MOVE COMU-CPFCNPJ-103(1:9)  TO WRK-4116E-CCNPJ-CPF.
           MOVE COMU-CPFCNPJ-103(11:4) TO WRK-4116E-CFLIAL-CNPJ.
           MOVE COMU-CPFCNPJ-103(16:2) TO WRK-4116E-CCTRL-CNPJ-CPF.

           MOVE 'DCOM4116'             TO WRK-MODULO.

           CALL WRK-MODULO             USING WRK-4116E-ENTRADA
                                             WRK-4116S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA


           EVALUATE WRK-4116S-COD-RETORNO
               WHEN '0000'
               WHEN '0002'
                   IF (WRK-4116S-CCONTR-LIM-DESC(11)  NOT NUMERIC) OR
                      (WRK-4116S-CCONTR-LIM-DESC(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9ITM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9ITM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9ITM
                       MOVE SG-MENSAGEM(61)
                                       TO WRK-OUT-MENSA-9ITM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO


               WHEN OTHER
                   MOVE COMU-CHAMADO-103        TO WRK-TELA
                   MOVE LENGTH OF COMU-AREA-103 TO COMU-LL-103
                   MOVE ZEROS                   TO COMU-COD-OPER-103
                   MOVE WRK-4116S-MSG-RETORNO   TO COMU-MENSA-103
                   MOVE 'DCOM3909'              TO COMU-TRANSACAO-103
                   MOVE WRK-CHNG                TO WRK-FUNCAO
                   MOVE COMU-AREA-103           TO WRK-MENSAGEM
           END-EVALUATE.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     FORMATACAO DAS OCORRENCIAS DA TELA                         *
      *----------------------------------------------------------------*
       2110-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (WRK-4116S-CCONTR-LIM-DESC(WRK-IND)  NOT NUMERIC) OR

              (WRK-4116S-CCONTR-LIM-DESC(WRK-IND)  EQUAL ZEROS)
               MOVE WRK-225          TO WRK-OUT-SELEC-ATTR-9ITM(WRK-IND)
           ELSE
               MOVE WRK-4116S-CCONTR-LIM-DESC(WRK-IND)
                                     TO WRK-OUT-NUMERO-9ITM(WRK-IND)
               MOVE WRK-4116S-VCONTR-LIM-DESC(WRK-IND)
                                     TO WRK-OUT-VLRLIM-9ITM(WRK-IND)
               MOVE WRK-4116S-VDISPN-CONTR-LIM(WRK-IND)
                                     TO WRK-OUT-VLRDIS-9ITM(WRK-IND)
               MOVE WRK-4116S-DINIC-CONTR-LIM(WRK-IND)
                                     TO WRK-OUT-DTINIC-9ITM(WRK-IND)
               INSPECT WRK-OUT-DTINIC-9ITM(WRK-IND)
                                        REPLACING ALL '.' BY '/'
               MOVE WRK-4116S-DVCTO-CONTR-LIM(WRK-IND)
                                     TO WRK-OUT-DTVENC-9ITM(WRK-IND)
               INSPECT WRK-OUT-DTVENC-9ITM(WRK-IND)
                                        REPLACING ALL '.' BY '/'
           END-IF.

      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     PROCESSAR DCOM9ITM                                         *
      *----------------------------------------------------------------*
       2200-PROCESSAR-DCOM9ITM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO WRK-INPUT-9ITM.

           PERFORM 2205-DEVOLVER-TELA-FIXO.

           EVALUATE TRUE

               WHEN WRK-INP-PFK-9ITM      EQUAL 'H' OR
                   (WRK-INP-PFK-9ITM      EQUAL '.' AND
                    WRK-INP-COMANDO-9ITM  EQUAL 'PFK01')
                    PERFORM 2210-DEVOLVER-TELA-VARIA
                    MOVE WRK-INP-MENSA-9ITM TO  WRK-OUT-MENSA-9ITM

               WHEN WRK-INP-PFK-9ITM      EQUAL '3' OR

                   (WRK-INP-PFK-9ITM      EQUAL '.' AND
                    WRK-INP-COMANDO-9ITM  EQUAL 'PFK03')
                    PERFORM 2220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-9ITM      EQUAL '7' OR
                   (WRK-INP-PFK-9ITM      EQUAL '.' AND
                    WRK-INP-COMANDO-9ITM  EQUAL 'PFK07')
                    PERFORM 2240-TELA-ANTERIOR

               WHEN WRK-INP-PFK-9ITM      EQUAL '8' OR
                   (WRK-INP-PFK-9ITM      EQUAL '.' AND
                    WRK-INP-COMANDO-9ITM  EQUAL 'PFK08')
                    PERFORM 2250-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-9ITM     EQUAL '.' AND
                     WRK-INP-COMANDO-9ITM EQUAL SPACES)
                     PERFORM 2270-TRATAR-ENTER
                     PERFORM 2210-DEVOLVER-TELA-VARIA

               WHEN  OTHER
                     MOVE SG-MENSAGEM(38)

                                       TO  WRK-OUT-MENSA-9ITM
                     PERFORM 2210-DEVOLVER-TELA-VARIA
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS FIXOS                                *
      *----------------------------------------------------------------*
       2205-DEVOLVER-TELA-FIXO         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-TMSTAMP-9ITM   TO WRK-OUT-TMSTAMP-9ITM.
           MOVE WRK-INP-CHAMADO-9ITM   TO WRK-OUT-CHAMADO-9ITM.
           MOVE WRK-INP-TPOPCAO-9ITM   TO WRK-OUT-TPOPCAO-9ITM.
           MOVE WRK-INP-ULTPAG-9ITM    TO WRK-OUT-ULTPAG-9ITM.
           MOVE WRK-INP-PROD-9ITM-X    TO WRK-OUT-PROD-9ITM-X.
           MOVE WRK-INP-SUBP-9ITM-X    TO WRK-OUT-SUBP-9ITM-X.
           MOVE WRK-INP-PAGINA-9ITM-X  TO WRK-OUT-PAGINA-9ITM-X.

           MOVE WRK-INP-MENSA-9ITM     TO WRK-OUT-MENSA-9ITM.

           MOVE WRK-INP-BANCO-9ITM-X   TO WRK-OUT-BANCO-9ITM-X.
           MOVE WRK-INP-AGENCIA-9ITM   TO WRK-OUT-AGENCIA-9ITM.
           MOVE WRK-INP-CONTA-9ITM-X   TO WRK-OUT-CONTA-9ITM-X.
           MOVE WRK-INP-CGCCPF-9ITM    TO WRK-OUT-CGCCPF-9ITM.
           MOVE WRK-INP-NOMECL-9ITM    TO WRK-OUT-NOMECL-9ITM.

      *----------------------------------------------------------------*
       2205-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     DEVOLVER TELA - DADOS VARIAVEIS                            *
      *----------------------------------------------------------------*
       2210-DEVOLVER-TELA-VARIA        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING  WRK-IND  FROM 1 BY 1
                     UNTIL  WRK-IND  GREATER 10


               IF (WRK-INP-NUMERO-9ITM(WRK-IND) NOT NUMERIC) OR
                  (WRK-INP-NUMERO-9ITM(WRK-IND) EQUAL ZEROS)
                   MOVE WRK-225      TO WRK-OUT-SELEC-ATTR-9ITM(WRK-IND)
               ELSE
                   MOVE WRK-INP-SELEC-9ITM(WRK-IND)
                                     TO WRK-OUT-SELEC-9ITM(WRK-IND)
                   MOVE WRK-INP-NUMERO-9ITM-X(WRK-IND)
                                     TO WRK-OUT-NUMERO-9ITM-X(WRK-IND)
                   MOVE WRK-INP-VLRLIM-9ITM-X(WRK-IND)
                                     TO WRK-OUT-VLRLIM-9ITM-X(WRK-IND)
                   MOVE WRK-INP-VLRDIS-9ITM-X(WRK-IND)
                                     TO WRK-OUT-VLRDIS-9ITM-X(WRK-IND)
                   MOVE WRK-INP-DTINIC-9ITM(WRK-IND)
                                     TO WRK-OUT-DTINIC-9ITM(WRK-IND)
                   MOVE WRK-INP-DTVENC-9ITM(WRK-IND)
                                     TO WRK-OUT-DTVENC-9ITM(WRK-IND)
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.

      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR AO PROGRAMA CHAMADOR - DCOM0102                   *
      *----------------------------------------------------------------*
       2220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF COMU-AREA-103
                                       TO COMU-LL-103

           MOVE 'DCOM3909'             TO COMU-TRANSACAO-103
           MOVE '3'                    TO COMU-PFK-103
           MOVE WRK-INP-SENHAS-9ITM    TO COMU-SENHAS-103
           MOVE WRK-INP-COMANDO-9ITM   TO COMU-COMANDO-103
           MOVE WRK-INP-TMSTAMP-9ITM   TO COMU-TIMESTAMP-103
           MOVE WRK-INP-CHAMADO-9ITM   TO COMU-CHAMADO-103
           MOVE WRK-INP-TPOPCAO-9ITM   TO COMU-TPOPCAO-103
           MOVE WRK-INP-NOMECL-9ITM    TO COMU-NOME-103
           STRING WRK-INP-CGCCPF-9ITM(01:3)
                  WRK-INP-CGCCPF-9ITM(05:3)

                  WRK-INP-CGCCPF-9ITM(09:3) '/'
                  WRK-INP-CGCCPF-9ITM(13:4) '-'
                  WRK-INP-CGCCPF-9ITM(18:2) DELIMITED BY SIZE
                                     INTO COMU-CPFCNPJ-103
           MOVE WRK-INP-BANCO-9ITM     TO COMU-BANCO-103
           MOVE WRK-INP-AGENCIA-9ITM   TO COMU-AGENCIA-103
           MOVE WRK-INP-CONTA-9ITM     TO COMU-CONTA-103-N

           MOVE WRK-INP-CHAMADO-9ITM   TO WRK-TELA
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE COMU-AREA-103          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *     RETORNAR TELA ANTERIOR                                     *
      *----------------------------------------------------------------*
       2240-TELA-ANTERIOR              SECTION.
      *----------------------------------------------------------------*


           IF  WRK-INP-PAGINA-9ITM  EQUAL 1
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MENSA-9ITM
               GO TO 2240-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-4116E-FLAG
           MOVE 'N'                    TO WRK-4116E-START-COUNT
           MOVE WRK-COD-DEPTO-N        TO WRK-COD-AGENC-N
           MOVE WRK-COD-AGENC-N        TO WRK-4116E-MSG-RETORNO(1:5)
           MOVE WRK-COD-USER           TO WRK-4116E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO WRK-4116E-CTERM
           MOVE WRK-INP-PROD-9ITM      TO WRK-4116E-CPRODT
           MOVE WRK-INP-SUBP-9ITM      TO WRK-4116E-CSPROD-DESC-COML
           MOVE WRK-INP-CONTA-9ITM     TO WRK-4116E-CCTA-BCRIA-CLI.
           MOVE WRK-INP-AGENCIA-9ITM(1:5)
                                       TO WRK-4116E-CAG-BCRIA
           MOVE WRK-INP-CGCCPF-9ITM(1:3)  TO WRK-4116E-CCNPJ-CPF(1:3)
           MOVE WRK-INP-CGCCPF-9ITM(5:3)  TO WRK-4116E-CCNPJ-CPF(4:3)
           MOVE WRK-INP-CGCCPF-9ITM(9:3)  TO WRK-4116E-CCNPJ-CPF(7:3)

           MOVE WRK-INP-CGCCPF-9ITM(13:4) TO WRK-4116E-CFLIAL-CNPJ.
           MOVE WRK-INP-CGCCPF-9ITM(18:2) TO WRK-4116E-CCTRL-CNPJ-CPF.
           COMPUTE WRK-4116E-RESTART  = (WRK-INP-PAGINA-9ITM - 2) * 10

           MOVE 'DCOM4116'             TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-4116E-ENTRADA
                                             WRK-4116S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4116S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4116S-CCONTR-LIM-DESC(11)  NOT NUMERIC) OR
                      (WRK-4116S-CCONTR-LIM-DESC(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9ITM
                       MOVE SG-MENSAGEM(17)
                                       TO WRK-OUT-MENSA-9ITM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9ITM
                       MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9ITM

                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4116S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9ITM
           END-EVALUATE.


           COMPUTE WRK-OUT-PAGINA-9ITM = WRK-INP-PAGINA-9ITM - 1.

      *----------------------------------------------------------------*
       2240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *     AVANCAR PARA TELA POSTERIOR                                *
      *----------------------------------------------------------------*
       2250-TELA-POSTERIOR             SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-ULTPAG-9ITM     EQUAL '*'
               PERFORM 2210-DEVOLVER-TELA-VARIA
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MENSA-9ITM
               GO TO 2250-99-FIM
           END-IF.

           MOVE 'N'                    TO WRK-4116E-FLAG
           MOVE 'N'                    TO WRK-4116E-START-COUNT
           MOVE WRK-COD-USER           TO WRK-4116E-CFUNC-BDSCO
           MOVE WRK-COD-DEPTO-N        TO WRK-COD-AGENC-N
           MOVE WRK-COD-AGENC-N        TO WRK-4116E-MSG-RETORNO(1:5)
           MOVE LNK-IO-LTERM           TO WRK-4116E-CTERM
           MOVE WRK-INP-PROD-9ITM      TO WRK-4116E-CPRODT
           MOVE WRK-INP-SUBP-9ITM      TO WRK-4116E-CSPROD-DESC-COML
           MOVE WRK-INP-CONTA-9ITM     TO WRK-4116E-CCTA-BCRIA-CLI.

           MOVE WRK-INP-AGENCIA-9ITM(1:5)
                                       TO WRK-4116E-CAG-BCRIA
           MOVE WRK-INP-CGCCPF-9ITM(1:3)  TO WRK-4116E-CCNPJ-CPF(1:3)
           MOVE WRK-INP-CGCCPF-9ITM(5:3)  TO WRK-4116E-CCNPJ-CPF(4:3)
           MOVE WRK-INP-CGCCPF-9ITM(9:3)  TO WRK-4116E-CCNPJ-CPF(7:3)
           MOVE WRK-INP-CGCCPF-9ITM(13:4) TO WRK-4116E-CFLIAL-CNPJ.
           MOVE WRK-INP-CGCCPF-9ITM(18:2) TO WRK-4116E-CCTRL-CNPJ-CPF.
           COMPUTE WRK-4116E-RESTART  = (WRK-INP-PAGINA-9ITM) * 10

           MOVE 'DCOM4116'             TO WRK-MODULO.
           CALL WRK-MODULO             USING WRK-4116E-ENTRADA
                                             WRK-4116S-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA

           EVALUATE WRK-4116S-COD-RETORNO
               WHEN '0000'
                   IF (WRK-4116S-CCONTR-LIM-DESC(11)  NOT NUMERIC) OR
                      (WRK-4116S-CCONTR-LIM-DESC(11)  EQUAL ZEROS)
                       MOVE '*'        TO WRK-OUT-ULTPAG-9ITM
                       MOVE SG-MENSAGEM(17)

                                       TO WRK-OUT-MENSA-9ITM
                   ELSE
                       MOVE SPACES     TO WRK-OUT-ULTPAG-9ITM
                       MOVE SG-MENSAGEM(01)
                                       TO WRK-OUT-MENSA-9ITM
                   END-IF
                   PERFORM 2110-MONTAR-TELA VARYING WRK-IND FROM 1 BY 1
                                              UNTIL WRK-IND GREATER 10

               WHEN '0099'
                   MOVE 'DB2'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   PERFORM 2210-DEVOLVER-TELA-VARIA
                   MOVE WRK-4116S-MSG-RETORNO
                                       TO WRK-OUT-MENSA-9ITM
           END-EVALUATE.


           COMPUTE WRK-OUT-PAGINA-9ITM = WRK-INP-PAGINA-9ITM + 1.


      *----------------------------------------------------------------*
       2250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2270-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS.

           PERFORM 2271-CONSISTIR-FISICA.

           IF  WRK-INCONSIS            EQUAL '*'
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

           PERFORM VARYING  WRK-IND  FROM 1 BY 1
                     UNTIL  WRK-IND  GREATER  10

               IF WRK-INP-SELEC-9ITM(WRK-IND) NOT EQUAL SPACES AND 'X'
                  MOVE '*'           TO WRK-INCONSIS
                  MOVE WRK-49369     TO WRK-OUT-SELEC-ATTR-9ITM(WRK-IND)
                  MOVE SG-MENSAGEM(41)
                                     TO WRK-OUT-MENSA-9ITM
               ELSE
                  IF WRK-INP-SELEC-9ITM(WRK-IND) EQUAL 'X'
                     MOVE WRK-49369  TO WRK-OUT-SELEC-ATTR-9ITM(WRK-IND)
                     ADD  1          TO WCT-SELEC
                     MOVE WRK-IND    TO WIND-SEL

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
                                       TO WRK-OUT-MENSA-9ITM
               ELSE
                 IF  WCT-SELEC         GREATER 1
                     MOVE SG-MENSAGEM(39)
                                       TO WRK-OUT-MENSA-9ITM
                 END-IF
               END-IF
           END-IF.


      *----------------------------------------------------------------*
       2271-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2272-TRANSFERIR-CONTROLE-0103   SECTION.
      *----------------------------------------------------------------*

           INITIALIZE COMU-AREA-103
           MOVE WRK-CHNG               TO WRK-FUNCAO

           MOVE LENGTH OF COMU-AREA-103
                                       TO COMU-LL-103
           MOVE 'DCOM3909'             TO COMU-TRANSACAO-103
           MOVE WRK-INP-SENHAS-9ITM    TO COMU-SENHAS-103
           MOVE WRK-INP-COMANDO-9ITM   TO COMU-COMANDO-103
           MOVE WRK-INP-TMSTAMP-9ITM   TO COMU-TIMESTAMP-103
           MOVE WRK-INP-TPOPCAO-9ITM   TO COMU-TPOPCAO-103
           MOVE WRK-INP-NOMECL-9ITM    TO COMU-NOME-103
           STRING WRK-INP-CGCCPF-9ITM(01:3)
                  WRK-INP-CGCCPF-9ITM(05:3)

                  WRK-INP-CGCCPF-9ITM(09:3) '/'
                  WRK-INP-CGCCPF-9ITM(13:4) '-'
                  WRK-INP-CGCCPF-9ITM(18:2) DELIMITED BY SIZE
                                     INTO COMU-CPFCNPJ-103
           MOVE WRK-INP-BANCO-9ITM     TO COMU-BANCO-103
           MOVE WRK-INP-AGENCIA-9ITM   TO COMU-AGENCIA-103
           MOVE WRK-INP-CONTA-9ITM     TO COMU-CONTA-103-N
           MOVE WRK-INP-NUMERO-9ITM(WIND-SEL)
                                       TO COMU-COD-OPER-103-N
           MOVE 'L'                    TO COMU-TP-OPER-103
           MOVE WRK-INP-CHAMADO-9ITM   TO WRK-TELA
           MOVE COMU-AREA-103          TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2272-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*


           MOVE LENGTH OF WRK-660-DCOM9ITM TO WRK-660-LL-AREA-9ITM.
           MOVE LENGTH OF WRK-OUTPUT-9ITM  TO WRK-OUT-LL-9ITM
                                              WRK-660-LL-MENSA-9ITM.

           CALL 'BRAD0660'             USING WRK-OUTPUT-9ITM
                                             WRK-660-DCOM9ITM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-9ITM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM3909'         TO ERR-PGM
           ELSE
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM3909'         TO ERR-MODULO
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
