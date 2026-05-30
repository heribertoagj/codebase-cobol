      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0409.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0409                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   05/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR OPERACAO - OPERACOES POR AGENCIA. *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDITM - CONSULTAR OPERACAO - OPERACOES POR AGENCIA.      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0402           CHAMA: DCOM0411            *
      *================================================================*
BRQ001*                   BRQ DIGITAL SOLUTIONS - ALTERACAO            *
BRQ001*----------------------------------------------------------------*
BRQ001*     ANALISTA....: LUCIANDRA SILVEIRA                           *
BRQ001*     ANALISTA DS.: RICARDO JAMMAL                               *
BRQ001*     DATA........: 23/01/2019                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*     OBJETIVO....: INCLUIR OPCAO 4 - CONSULTAR PENDENTE         *
BRQ001*                   PROJETO 18/0456 - BORDERO ELETRONICO         *
BRQ001*================================================================*

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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0409 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- ALPHA    - NORMAL    - PROTEGIDO    - NAO CURSOR - ALFA ---*
       01  WRK-225                     PIC S9(008) COMP    VALUE +225.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** VARIAVEIS AUXILIARES ***'.
      *----------------------------------------------------------------*
       01  WRK-AUXILIARES.
           05  WRK-IND                 PIC  9(002)         VALUE ZEROS.
           05  WRK-QTD-OPCAO-D         PIC  9(001)         VALUE ZEROS.
           05  WRK-SQLCA               PIC  X(136)         VALUE SPACES.
           05  WRK-MODULO              PIC  X(008)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.
           05  WRK-FLAG-ERRO           PIC  X(001)         VALUE 'N'.
           05  WRK-MENSAGEM-ERRO       PIC  X(079)         VALUE SPACES.
           05  WRK-MENSA-PF7           PIC  X(079) VALUE
               'PF7 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA VOLTAR'.
           05  WRK-MENSA-PF8           PIC  X(079) VALUE
               'PF8 DESPREZARA AS ATUALIZACOES EFETUADAS - TECLE NOVAMEN
      -        'TE PARA AVANCAR'.

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREAS AUXILIARES PARA POOL5005 ***'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)         VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)         VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)         VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)         VALUE SPACES.
           05  WRK-TELA                PIC  X(008)         VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)         VALUE
               'VRS002'.
           05  WRK-COD-USER            PIC  X(007)         VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).
           05  WRK-COD-DEPTO.
               10  FILLER              PIC  X(002)         VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)         VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)         VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(1988)        VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ERRO DA BRAD0660 ***'.
      *----------------------------------------------------------------*
       01 WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)         VALUE
              '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)         VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(011)         VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)         VALUE ZEROS.
           05 FILLER                   PIC  X(002)         VALUE ' *'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDITM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DITM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DITM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DITM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DITM     PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DITM.
BRQ001         10  WRK-INP-TITULO-DITM  PIC  X(054) VALUE SPACES.
               10  WRK-INP-FIMAMOS-DITM PIC  X(001) VALUE SPACES.
BRQ001         10  WRK-INP-OPCAO-DITM   PIC  X(001) VALUE SPACES.
               10  WRK-INP-TMSTAMP-DITM PIC  X(026) VALUE SPACES.
               10  WRK-INP-PAGINA-DITM-X.
                   15  WRK-INP-PAGINA-DITM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-CODAGE-DITM-X.
                   15  WRK-INP-CODAGE-DITM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-INP-DESCAGE-DITM PIC  X(020) VALUE SPACES.
               10  WRK-INP-TABELA-DITM    OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DITM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-OPERA-DITM-X.
                       20  WRK-INP-OPERA-DITM PIC  9(013) VALUE ZEROS.
                   15  WRK-INP-PROD-DITM      PIC  X(012) VALUE SPACES.
                   15  WRK-INP-SUBPROD-DITM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTOPER-DITM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-VALOR-DITM     PIC  X(017) VALUE SPACES.
                   15  WRK-INP-MEIOENT-DITM   PIC  X(015) VALUE SPACES.
                   15  WRK-INP-SITANAL-DITM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-PARECER-DITM   PIC  X(003) VALUE SPACES.
           05  WRK-INP-MENSA-DITM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDITM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DITM.
BRQ001     05  WRK-OUT-LL-DITM          PIC S9(004) COMP VALUE ZEROS.
           05  WRK-OUT-ZZ-DITM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DITM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DITM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DITM.
BRQ001         10  WRK-OUT-TITULO-DITM  PIC  X(054) VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DITM PIC  X(001) VALUE SPACES.
BRQ001         10  WRK-OUT-OPCAO-DITM   PIC  X(001) VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DITM PIC  X(026) VALUE SPACES.
               10  WRK-OUT-PAGINA-DITM-X.
                   15  WRK-OUT-PAGINA-DITM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-CODAGE-DITM-X.
                   15  WRK-OUT-CODAGE-DITM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-DESCAGE-DITM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-TABELA-DITM    OCCURS  05  TIMES.
                   15  WRK-OUT-SELEC-ATTR-DITM PIC  9(04) COMP.
                   15  WRK-OUT-SELEC-DITM      PIC  X(001) VALUE SPACES.
                   15  WRK-OUT-OPERA-DITM-X.
                       20  WRK-OUT-OPERA-DITM  PIC  9(013) VALUE ZEROS.
                   15  WRK-OUT-PROD-DITM       PIC  X(012) VALUE SPACES.
                   15  WRK-OUT-SUBPROD-DITM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-DTOPER-DITM     PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-VALOR-DITM-X.
                       20  WRK-OUT-VALOR-DITM  PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                   15  WRK-OUT-MEIOENT-DITM    PIC  X(015) VALUE SPACES.
                   15  WRK-OUT-SITANAL-DITM    PIC  X(010) VALUE SPACES.
                   15  WRK-OUT-PARECER-DITM    PIC  X(003) VALUE SPACES.
               10  WRK-OUT-MENSA-DITM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDITM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDITM.
BRQ001     05  WRK-660-LL-AREA-DITM     PIC  9(004) COMP   VALUE 0000.
BRQ001     05  WRK-660-LL-MENSAGEM-DITM PIC  9(004) COMP   VALUE 0000.
           05  WRK-660-SENHAS-DITM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DITM     PIC  9(004) COMP   VALUE 0068.
BRQ001     05  WRK-660-TITULO-DITM      PIC  9(004) COMP   VALUE 0054.
           05  WRK-660-FIMAMOS-DITM     PIC  9(004) COMP   VALUE 0001.
BRQ001     05  WRK-660-OPCAO-DITM       PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TMSTAMP-DITM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-PAGINA-DITM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-CODAGE-DITM      PIC  9(004) COMP   VALUE 0005.
           05  WRK-660-DESCAGE-DITM     PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-TABELA-DITM    OCCURS  05  TIMES.
               10  WRK-660-SELEC-DITM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-OPERA-DITM   PIC  9(004) COMP   VALUE 0013.
               10  WRK-660-PROD-DITM    PIC  9(004) COMP   VALUE 0012.
               10  WRK-660-SUBPROD-DITM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTOPER-DITM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-VALOR-DITM   PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-MEIOENT-DITM PIC  9(004) COMP   VALUE 0015.
               10  WRK-660-SITANAL-DITM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-PARECER-DITM PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-MENSA-DITM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0402 ***'.
      *----------------------------------------------------------------*

       01  COMU-AREA-UN.
           05  COMU-LL-UN              PIC S9(004) COMP  VALUE +297.
           05  COMU-ZZ-UN              PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE-UN.
               10  COMU-TRANSACAO-UN   PIC  X(008)       VALUE SPACES.
               10  FILLER-UN           PIC  X(006)       VALUE SPACES.
               10  COMU-PFK-UN         PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS-UN          PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO-UN         PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS-UN.
BRQ001         10  COMU-OPCAO-UN       PIC  X(001)       VALUE SPACES.
               10  COMU-OPER-UN        PIC  9(013)       VALUE ZEROS.
               10  COMU-AGENCIA-UN     PIC  9(005)       VALUE ZEROS.
               10  COMU-AGENC-UN       PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA-UN       PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CNPJ-UN        PIC  9(009)       VALUE ZEROS.
FS2511         10  COMU-CNPJ-UN        PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL-UN      PIC  9(004)       VALUE ZEROS.
FS2511         10  COMU-FILIAL-UN      PIC  X(004)       VALUE SPACES.
               10  COMU-DIG-UN         PIC  9(002)       VALUE ZEROS.
               10  COMU-OPERACAO-UN.
                   15 COMU-DANO-OPER-UN PIC 9(004)       VALUE ZEROS.
                   15 COMU-NSEQ-OPER-UN PIC 9(009)       VALUE ZEROS.
               10  COMU-TIMESTAMP-UN   PIC  X(026)       VALUE ZEROS.
               10  COMU-MENSAGEM-UN    PIC  X(079)       VALUE SPACES.
               10  COMU-PAGINA-UN      PIC  9(003)       VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4710 **'.
      *----------------------------------------------------------------*

       01  4710-ENTRADA-ROTEADOR.
           03  4710E-HEADER.
               05  4710E-COD-RETORNO              PIC  X(004).
               05  4710E-MSG-RETORNO              PIC  X(079).
               05  4710E-RESTART                  PIC  9(005).
               05  4710E-FLAG                     PIC  9(001).
               05  4710E-START-COUNT              PIC  X(001).
               05  4710E-CFUNC-BDSCO              PIC  9(009).
               05  4710E-CTERM                    PIC  X(008).
           03  4710E-DADOS.
               10  4710E-FLAG-FILTRO              PIC  X(002).
               10  4710E-CBCO                     PIC  9(003).
               10  4710E-CAG-BCRIA                PIC  9(005).
               10  4710E-CAG-OPER                 PIC  9(005).
               10  4710E-CCTA-BCRIA-CLI           PIC  9(013).
               10  4710E-CPRODT                   PIC  9(003).
               10  4710E-CSPROD-DESC-COML         PIC  9(003).
               10  4710E-DINIC-OPER-DESC          PIC  X(010).
               10  4710E-CELMTO-DESC-COML         PIC  9(003).
FS2511*        10  4710E-CCNPJ-CPF                PIC  9(009).
FS2511         10  4710E-CCNPJ-CPF                PIC  X(009).
FS2511*        10  4710E-CFLIAL-CNPJ              PIC  9(004).
FS2511         10  4710E-CFLIAL-CNPJ              PIC  X(004).
               10  4710E-CCTRL-CNPJ-CPF           PIC  9(002).
               10  4710E-CSIT-DESC-COML           PIC  9(003).
               10  4710E-CINDCD-EVNTO-WORKFLOW    PIC  9(003).
               10  4710E-CINDCD-SIT-FINAL-EVNTO   PIC  9(003).

       01  4710-SAIDA-ROTEADOR.
           03  4710S-HEADER.
               05  4710S-COD-RETORNO       PIC  X(004).
               05  4710S-MSG-RETORNO       PIC  X(079).
               05  4710S-RESTART           PIC  9(005).
               05  4710S-FLAG              PIC  X(001).
               05  4710S-QTDE-RETORNADA    PIC  9(003).
               05  4710S-COUNT             PIC  9(009).

           03  4710S-DADOS.
               05  4710S-TABELA-SAIDA      OCCURS 10  TIMES.
                   10  4710S-DANO-OPER-DESC         PIC  9(004).
                   10  4710S-NSEQ-OPER-DESC         PIC  9(009).
                   10  4710S-IPSSOA-DESC-COML       PIC  X(060).
                   10  4710S-CBCO                   PIC  9(003).
                   10  4710S-CAG-BCRIA              PIC  9(005).
                   10  4710S-CAG-BCRIA-DESC         PIC  X(040).
                   10  4710S-CCTA-BCRIA-CLI         PIC  9(013).
                   10  4710S-CBCO-OPER-OPER         PIC  9(003).
                   10  4710S-CAG-OPER-OPER          PIC  9(005).
                   10  4710S-CSIT-DESC-COML         PIC  9(003).
                   10  4710S-IRSUMO-SIT-DESC        PIC  X(015).
                   10  4710S-CSGL-SIT-DESC          PIC  X(006).
                   10  4710S-CPRODT                 PIC  9(003).
                   10  4710S-IABREV-PRODT           PIC  X(012).
                   10  4710S-CSGL-PRODT-DESC        PIC  X(006).
                   10  4710S-CSPROD-DESC-COML       PIC  9(003).
                   10  4710S-CTPO-DESC-COML         PIC  9(003).
                   10  4710S-IRSUMO-SPROD-DESC      PIC  X(010).
                   10  4710S-CSGL-SPROD-DESC        PIC  X(006).
                   10  4710S-VOPER-DESC-COML        PIC  9(015)V9(2).
                   10  4710S-DINIC-OPER-DESC        PIC  X(010).
                   10  4710S-DVCTO-FNAL-OPER        PIC  X(010).
                   10  4710S-CMEIO-ENTRD-DESC       PIC  9(03).
                   10  4710S-IMEIO-ENTRD-DESC       PIC  X(25).
                   10  4710S-IRSUMO-MEIO-ENTRD      PIC  X(15).
                   10  4710S-CSGL-MEIO-ENTRD        PIC  X(06).
                   10  4710S-CINDCD-LIBRC-ANTCP     PIC  X(01).
                   10  4710S-PARECER-FORML          PIC  X(01).
                   10  4710S-VLIQ-OPER-DESC         PIC  9(015)V9(2).

           03  4710S-CONSISTENCIA REDEFINES 4710S-DADOS.
               05  4710S-TAMANHO-ERRO      PIC  9(001).
               05  4710S-TABELA-ERROS      OCCURS  2 TIMES.
                   07  4710S-CAMPOS-ERRO   PIC  9(001).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0409 - FIM DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01 LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01 LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY 'DLITCBL'             USING LNK-IO-PCB
                                             LNK-ALT-PCB.

           MOVE WRK-GU                 TO WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
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

           IF  RETURN-CODE          EQUAL  04 OR 12
               PERFORM 2000-FINALIZAR
           END-IF.

           MOVE SPACES                 TO  WRK-OUT-DADOS-DITM
                                           WRK-OUT-SENHAS-DITM
                                           WRK-INCONSISTENCIA.

           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROGRAMA A MENSAGEM                               *
      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE 'DCOMDITM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0409'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDITM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDITM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DE OUTRAS TRANSACOES            *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           MOVE WRK-MENSAGEM           TO  COMU-AREA-UN.

BRQ001     IF  COMU-OPCAO-UN           EQUAL '3'
BRQ001         MOVE '   CONSULTAR LIBERACAO - OPERACOES POR AGENCIA
BRQ001-             '   '              TO WRK-OUT-TITULO-DITM
BRQ001     ELSE
BRQ001         IF  COMU-OPCAO-UN       EQUAL '4'
BRQ001             MOVE 'CONSULTAR PENDENTES NOS CANAIS - OPERACOES POR
BRQ001-                 'AGENCIA'      TO WRK-OUT-TITULO-DITM
BRQ001         END-IF
BRQ001     END-IF.
BRQ001
BRQ001     MOVE COMU-OPCAO-UN          TO  WRK-INP-OPCAO-DITM
BRQ001                                     WRK-OUT-OPCAO-DITM
BRQ001
           MOVE COMU-TIMESTAMP-UN      TO  WRK-INP-TMSTAMP-DITM
                                           WRK-OUT-TMSTAMP-DITM

           MOVE COMU-AGENCIA-UN        TO  WRK-INP-CODAGE-DITM
                                           WRK-OUT-CODAGE-DITM
                                           4710E-CAG-BCRIA

           IF  WRK-TRANSACAO           EQUAL 'DCOM0411'
               MOVE COMU-PAGINA-UN     TO WRK-OUT-PAGINA-DITM
               COMPUTE 4710E-RESTART = (COMU-PAGINA-UN - 1) * 5
               PERFORM 1110-ACESSAR-DCOM4710
               IF  4710S-COD-RETORNO   EQUAL '0003'
               AND COMU-PAGINA-UN      GREATER 1
                   INITIALIZE 4710-ENTRADA-ROTEADOR
                              4710-SAIDA-ROTEADOR
                   MOVE 1              TO WRK-OUT-PAGINA-DITM
                   MOVE COMU-AGENCIA-UN
                                       TO 4710E-CAG-BCRIA
                   MOVE ZEROS          TO 4710E-RESTART
                   PERFORM 1110-ACESSAR-DCOM4710
               END-IF
           ELSE
               MOVE 1                  TO WRK-OUT-PAGINA-DITM
               MOVE ZEROS              TO 4710E-RESTART
           END-IF.

           PERFORM 1110-ACESSAR-DCOM4710.

           IF  4710S-COD-RETORNO       EQUAL '0003'
               MOVE 'NAO EXISTE(M) OPERACAO(OES) PARA EFETUAR CONSULTA'
                                       TO  WRK-MENSAGEM-ERRO
               MOVE 'S'                TO  WRK-FLAG-ERRO
               PERFORM 1220-RETORNAR-CHAMADOR
           END-IF.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM4710           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  4710E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  4710E-CTERM
           MOVE 237                    TO  4710E-CBCO
           MOVE 'N'                    TO  4710E-START-COUNT
           MOVE ZEROS                  TO  4710E-FLAG

BRQ001     IF  WRK-INP-OPCAO-DITM      EQUAL '3'
               MOVE 'CL'               TO  4710E-FLAG-FILTRO
BRQ001     ELSE
BRQ001         IF  WRK-INP-OPCAO-DITM  EQUAL '4'
BRQ001             MOVE 'CM'           TO  4710E-FLAG-FILTRO
BRQ001         END-IF
BRQ001     END-IF.

           MOVE 'DCOM4710'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    4710-ENTRADA-ROTEADOR
                                       4710-SAIDA-ROTEADOR
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE 4710S-COD-RETORNO
               WHEN '0000'
                    IF 4710S-DANO-OPER-DESC(6) EQUAL ZEROS
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DITM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DITM
                    END-IF

                    MOVE 'SELECIONE COM "D" PARA DETALHAR'
                                    TO  WRK-OUT-MENSA-DITM

                    PERFORM 1111-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

                    MOVE 4710S-CAG-BCRIA-DESC(1)
                                    TO WRK-OUT-DESCAGE-DITM

               WHEN '0003'
                   GO TO 1110-99-FIM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.

      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1111-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF 4710S-DANO-OPER-DESC(WRK-IND) IS NUMERIC
           AND 4710S-DANO-OPER-DESC(WRK-IND) NOT EQUAL ZEROS
              MOVE 4710S-DANO-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DITM(WRK-IND)(1:4)
              MOVE 4710S-NSEQ-OPER-DESC(WRK-IND)
                                  TO WRK-OUT-OPERA-DITM(WRK-IND)(5:9)
              MOVE 4710S-IABREV-PRODT(WRK-IND)
                                  TO WRK-OUT-PROD-DITM(WRK-IND)
              MOVE 4710S-IRSUMO-SPROD-DESC(WRK-IND)
                                  TO WRK-OUT-SUBPROD-DITM(WRK-IND)
              STRING 4710S-DINIC-OPER-DESC(WRK-IND)(1:2)
              '/'    4710S-DINIC-OPER-DESC(WRK-IND)(4:2)
              '/'    4710S-DINIC-OPER-DESC(WRK-IND)(7:4)
              DELIMITED BY SIZE   INTO WRK-OUT-DTOPER-DITM(WRK-IND)
              MOVE 4710S-VOPER-DESC-COML(WRK-IND)
                                  TO WRK-OUT-VALOR-DITM(WRK-IND)
              MOVE 4710S-IRSUMO-MEIO-ENTRD(WRK-IND)
                                  TO WRK-OUT-MEIOENT-DITM(WRK-IND)
              MOVE 4710S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITANAL-DITM(WRK-IND)
              IF  4710S-PARECER-FORML(WRK-IND) EQUAL 'S'
                  MOVE 'SIM'      TO WRK-OUT-PARECER-DITM(WRK-IND)
              ELSE
                  MOVE 'NAO'      TO WRK-OUT-PARECER-DITM(WRK-IND)
              END-IF
           ELSE

              MOVE WRK-225        TO WRK-OUT-SELEC-ATTR-DITM(WRK-IND)

           END-IF.

      *----------------------------------------------------------------*
       1111-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDITM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDITM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DITM.
           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DITM     EQUAL 'H' OR
                   (WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL 'PFK1')
                    MOVE WRK-INP-MENSA-DITM TO WRK-OUT-MENSA-DITM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DITM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DITM
                                                               (WRK-IND)
                        END-IF
                    END-PERFORM

               WHEN WRK-INP-PFK-DITM     EQUAL '3' OR
                   (WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL 'PFK3')
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DITM     EQUAL 'X' OR
                   (WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL 'PFK5')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DITM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-DITM     EQUAL '7' OR
                   (WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL 'PFK7')
                    PERFORM 1260-TRATAR-VOLTA-PAG

               WHEN WRK-INP-PFK-DITM     EQUAL '8' OR
                   (WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL 'PFK8')
                    PERFORM 1270-TRATAR-AVANCA-PAG

               WHEN WRK-INP-PFK-DITM     EQUAL '.' AND
                    WRK-INP-COMANDO-DITM EQUAL  SPACES OR LOW-VALUES
                    PERFORM 1280-TRATAR-ENTER
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DITM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DITM
                                                               (WRK-IND)
                        END-IF
                    END-PERFORM

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DITM
                    PERFORM VARYING WRK-IND FROM 1 BY 1
                              UNTIL WRK-IND GREATER 5
                        IF  WRK-INP-OPERA-DITM-X(WRK-IND)
                                         EQUAL SPACES OR LOW-VALUES
                            MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DITM
                                                               (WRK-IND)
                        END-IF
                    END-PERFORM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA DEVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

BRQ001     MOVE WRK-INP-TITULO-DITM    TO WRK-OUT-TITULO-DITM.
           MOVE WRK-INP-FIMAMOS-DITM   TO WRK-OUT-FIMAMOS-DITM.
BRQ001     MOVE WRK-INP-OPCAO-DITM     TO WRK-OUT-OPCAO-DITM.
           MOVE WRK-INP-PAGINA-DITM-X  TO WRK-OUT-PAGINA-DITM-X.
           MOVE WRK-INP-CODAGE-DITM-X  TO WRK-OUT-CODAGE-DITM-X.
           MOVE WRK-INP-DESCAGE-DITM   TO WRK-OUT-DESCAGE-DITM.
           MOVE WRK-INP-TMSTAMP-DITM   TO WRK-OUT-TMSTAMP-DITM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
              MOVE WRK-INP-SELEC-DITM(WRK-IND)
                                      TO WRK-OUT-SELEC-DITM(WRK-IND)
              MOVE WRK-INP-OPERA-DITM-X(WRK-IND)
                                      TO WRK-OUT-OPERA-DITM-X(WRK-IND)
              MOVE WRK-INP-PROD-DITM(WRK-IND)
                                      TO WRK-OUT-PROD-DITM(WRK-IND)
              MOVE WRK-INP-SUBPROD-DITM(WRK-IND)
                                      TO WRK-OUT-SUBPROD-DITM(WRK-IND)
              MOVE WRK-INP-DTOPER-DITM(WRK-IND)
                                      TO WRK-OUT-DTOPER-DITM(WRK-IND)
              MOVE WRK-INP-VALOR-DITM(WRK-IND)
                                      TO WRK-OUT-VALOR-DITM-X(WRK-IND)
              MOVE WRK-INP-MEIOENT-DITM(WRK-IND)
                                      TO WRK-OUT-MEIOENT-DITM(WRK-IND)
              MOVE WRK-INP-SITANAL-DITM(WRK-IND)
                                      TO WRK-OUT-SITANAL-DITM(WRK-IND)
              MOVE WRK-INP-PARECER-DITM(WRK-IND)
                                      TO WRK-OUT-PARECER-DITM(WRK-IND)
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  COMU-AREA-UN.

           IF  WRK-FLAG-ERRO         EQUAL 'S'
               MOVE WRK-MENSAGEM-ERRO
                                     TO COMU-MENSAGEM-UN
           END-IF.

           MOVE LENGTH OF COMU-AREA-UN
                                     TO COMU-LL-UN
           MOVE ZEROS                TO COMU-ZZ-UN

           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0402'           TO WRK-TELA.
           MOVE 'DCOM0409'           TO COMU-TRANSACAO-UN.
           MOVE WRK-INP-SENHAS-DITM  TO COMU-SENHAS-UN.
           MOVE WRK-INP-TMSTAMP-DITM TO COMU-TIMESTAMP-UN.
           MOVE COMU-AREA-UN         TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG            TO WRK-FUNCAO.
           MOVE 'DCOM0409'          TO WRK-TELA
                                       COMU-TRANSACAO-UN.
           MOVE '5'                 TO COMU-PFK-UN.
           MOVE COMU-AREA-UN        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0409'         TO WRK-TELA
                                      COMU-TRANSACAO-UN.
           MOVE 'A'                TO COMU-PFK-UN.
           MOVE COMU-AREA-UN       TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0411              *
      *----------------------------------------------------------------*
       1250-CHAMAR-TRAN-DCOM0411       SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0411'                  TO WRK-TELA.
           MOVE 'DCOM0409'                  TO COMU-TRANSACAO-UN
           MOVE WRK-INP-SENHAS-DITM         TO COMU-SENHAS-UN
           MOVE WRK-INP-TMSTAMP-DITM        TO COMU-TIMESTAMP-UN.
           MOVE WRK-INP-PAGINA-DITM         TO COMU-PAGINA-UN.
BRQ001     MOVE WRK-INP-OPCAO-DITM          TO COMU-OPCAO-UN.

           MOVE WRK-INP-CODAGE-DITM         TO COMU-AGENCIA-UN
           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
               IF  WRK-INP-SELEC-DITM(WRK-IND) EQUAL 'D'
                   MOVE WRK-INP-OPERA-DITM(WRK-IND) TO COMU-OPERACAO-UN
               END-IF
           END-PERFORM.

           MOVE COMU-AREA-UN                TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1250-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1260-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DITM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DITM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                  IF  WRK-INP-OPERA-DITM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DITM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1260-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DITM      NOT EQUAL WRK-MENSA-PF7
               IF  WRK-INP-SELEC-DITM(1) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(2) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(3) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(4) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(5) EQUAL 'D'
                   MOVE WRK-MENSA-PF7  TO WRK-OUT-MENSA-DITM
                   GO TO 1260-99-FIM
               END-IF
           END-IF.

           MOVE SPACES                 TO WRK-OUT-FIMAMOS-DITM
                                          WRK-OUT-SELEC-DITM(1)
                                          WRK-OUT-SELEC-DITM(2)
                                          WRK-OUT-SELEC-DITM(3)
                                          WRK-OUT-SELEC-DITM(4)
                                          WRK-OUT-SELEC-DITM(5).

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           MOVE WRK-INP-CODAGE-DITM    TO 4710E-CAG-BCRIA

           MOVE WRK-COD-USER           TO  4710E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  4710E-CTERM
           MOVE 237                    TO  4710E-CBCO
           MOVE 'N'                    TO  4710E-START-COUNT
           MOVE ZEROS                  TO  4710E-FLAG
           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DITM - 2) * 5

BRQ001     IF  WRK-INP-OPCAO-DITM      EQUAL '3'
               MOVE 'CL'               TO  4710E-FLAG-FILTRO
BRQ001     ELSE
BRQ001         IF  WRK-INP-OPCAO-DITM  EQUAL '4'
BRQ001             MOVE 'CM'           TO  4710E-FLAG-FILTRO
BRQ001         END-IF
BRQ001     END-IF.

           MOVE 'DCOM4710'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    4710-ENTRADA-ROTEADOR
                                       4710-SAIDA-ROTEADOR
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE 4710S-COD-RETORNO
               WHEN '0000'
                    IF 4710S-DANO-OPER-DESC(6) EQUAL ZEROS
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DITM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DITM
                    END-IF

                    MOVE 'SELECIONE COM "D" PARA DETALHAR'
                                    TO  WRK-OUT-MENSA-DITM
                    PERFORM 1111-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  'APL'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-EVALUATE.


           COMPUTE WRK-OUT-PAGINA-DITM = WRK-INP-PAGINA-DITM - 1.

           MOVE 'AMOSTRAGEM CONTINUA'  TO  WRK-OUT-MENSA-DITM.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1270-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DITM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DITM
              PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER 5
                  IF  WRK-INP-OPERA-DITM-X(WRK-IND)
                                       EQUAL SPACES OR LOW-VALUES
                      MOVE WRK-225 TO WRK-OUT-SELEC-ATTR-DITM(WRK-IND)
                  END-IF
              END-PERFORM
              GO TO 1270-99-FIM
           END-IF.

           IF  WRK-INP-MENSA-DITM      NOT EQUAL WRK-MENSA-PF8
               IF  WRK-INP-SELEC-DITM(1) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(2) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(3) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(4) EQUAL 'D'
               OR  WRK-INP-SELEC-DITM(5) EQUAL 'D'
                   MOVE WRK-MENSA-PF8  TO WRK-OUT-MENSA-DITM
                   GO TO 1270-99-FIM
               END-IF
           END-IF

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
               INITIALIZE WRK-OUT-TABELA-DITM(WRK-IND)
               MOVE SPACES             TO WRK-OUT-OPERA-DITM-X(WRK-IND)
                                          WRK-OUT-VALOR-DITM-X(WRK-IND)
           END-PERFORM.

           INITIALIZE 4710-ENTRADA-ROTEADOR
                      4710-SAIDA-ROTEADOR.

           MOVE WRK-INP-CODAGE-DITM    TO 4710E-CAG-BCRIA
           COMPUTE 4710E-RESTART = (WRK-INP-PAGINA-DITM) * 5

           PERFORM 1110-ACESSAR-DCOM4710.

           COMPUTE WRK-OUT-PAGINA-DITM = WRK-INP-PAGINA-DITM + 1.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           IF (WRK-INP-SELEC-DITM(1)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DITM(2)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DITM(3)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DITM(4)   EQUAL SPACES OR LOW-VALUES)
           AND(WRK-INP-SELEC-DITM(5)   EQUAL SPACES OR LOW-VALUES)
               MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                                       TO WRK-OUT-MENSA-DITM
               MOVE '*'                TO WRK-INCONSISTENCIA
               GO TO 1280-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DITM(WRK-IND) NOT EQUAL 'D' AND
                                         SPACES AND    LOW-VALUES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DITM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS: "D"'
                                 TO  WRK-OUT-MENSA-DITM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1280-99-FIM
           END-IF.


           MOVE    ZEROS         TO WRK-QTD-OPCAO-D

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DITM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D EQUAL  ZEROS
              MOVE 'SELECIONE COM "D" PARA DETALHAR' TO
                                        WRK-OUT-MENSA-DITM
           ELSE
             IF WRK-QTD-OPCAO-D  GREATER  1
                PERFORM VARYING WRK-IND FROM 1 BY 1
                        UNTIL WRK-IND GREATER  5
                  IF WRK-INP-SELEC-DITM(WRK-IND) EQUAL 'D'
                   MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DITM(WRK-IND)
                  END-IF
                END-PERFORM
                MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                              TO  WRK-OUT-MENSA-DITM
             ELSE
                PERFORM 1250-CHAMAR-TRAN-DCOM0411
           END-IF.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH                 OF WRK-OUTPUT-DITM
                                       TO WRK-660-LL-MENSAGEM-DITM
                                          WRK-OUT-LL-DITM
           MOVE LENGTH                 OF WRK-660-DCOMDITM
                                       TO WRK-660-LL-AREA-DITM

           CALL 'BRAD0660'             USING WRK-OUTPUT-DITM
                                             WRK-660-DCOMDITM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DITM        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FINALIZA O PROCESSAMENTO DO PROGRAMA              *
      *----------------------------------------------------------------*
       2000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA FORMATA E EMITE A MENSAGEM DE ERRO DO PROGRAMA    *
      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF  ERR-MODULO              EQUAL SPACES AND LOW-VALUES
               MOVE 'SENHAS02'         TO ERR-PGM
               MOVE 'DCOM0409'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0409'         TO ERR-PGM
           END-IF.

           MOVE WRK-COD-USER           TO ERR-COD-USER.
           MOVE WRK-COD-DEPTO          TO ERR-COD-DEPTO.


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

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
