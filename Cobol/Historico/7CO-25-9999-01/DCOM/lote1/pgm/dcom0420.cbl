      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID. DCOM0420.
       AUTHOR.     MARCELO MORINA.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0420                                    *
      *    PROGRAMADOR.:   MARCELO MORINA MARQUES                      *
      *    ANALISTA....:   MARCELO MORINA MARQUES                      *
      *    DATA........:   07/02/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CONSULTAR CONTRATO - CONTRATOS POR AGENCIA. *
      *----------------------------------------------------------------*
      *    TELAS.......:                                               *
      *    DCOMDTTM - CONSULTAR CONTRATO - CONTRATOS POR AGENCIA.      *
      *----------------------------------------------------------------*
      *    INC'S.......:                                               *
      *    I#BRAD7C - AREA DE ERRO DA BRAD7100.                        *
      *    I#DCOMSG - TABELA DE MSG DE RETORNO DO SISTEMA DCOM(ONLINE).*
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA  *
      *    BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO     *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC      *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *    CHAMADO POR.: DCOM0413           CHAMA: DCOM0419            *
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
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0420 - INICIO DA AREA DE WORKING ***'.
      *----------------------------------------------------------------*

      *---------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE ATRIBUTOS ***'.
      *---------------------------------------------------------------*
      *--- NUMERIC  - NORMAL    - PROTEGIDO    - NAO CURSOR - NUM  ---*
       01  WRK-241                     PIC S9(008) COMP    VALUE +241.
      *--- NUMERICO - NORMAL    - PROTEGIDO    - POS CURSOR - NUM  ---*
       01  WRK-49361                   PIC S9(008) COMP    VALUE +49361.
      *--- NUMERICO - BRILHANTE - DESPROTEGIDO - POS CURSOR - NUM  ---*
       01  WRK-49369                   PIC S9(008) COMP    VALUE +49369.
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
           05  WRK-SELEC-ANT           PIC  X(001)         VALUE SPACES.
           05  WRK-INCONSISTENCIA      PIC  X(001)         VALUE SPACES.

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
           '*** AREA DA TELA DCOMDTTM  - INPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-DTTM.
           05  FILLER                  PIC  X(018) VALUE SPACES.
           05  WRK-INP-PFK-DTTM        PIC  X(001) VALUE SPACES.
           05  WRK-INP-SENHAS-DTTM     PIC  X(037) VALUE SPACES.
           05  WRK-INP-COMANDO-DTTM    PIC  X(068) VALUE SPACES.
           05  WRK-INP-DADOS-DTTM.
               10  WRK-INP-TPOOPC-DTTM  PIC  X(001) VALUE SPACES.
               10  WRK-INP-TMSTAMP-DTTM PIC  X(026) VALUE SPACES.
               10  WRK-INP-FIMAMOS-DTTM PIC  X(001) VALUE SPACES.
               10  WRK-INP-PAGINA-DTTM-X.
                   15  WRK-INP-PAGINA-DTTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-INP-CODAGE-DTTM-X.
                   15  WRK-INP-CODAGE-DTTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-INP-DESCAGE-DTTM PIC  X(020) VALUE SPACES.
               10  WRK-INP-TABELA-DTTM    OCCURS  05  TIMES.
                   15  WRK-INP-SELEC-DTTM     PIC  X(001) VALUE SPACES.
                   15  WRK-INP-CONTR-DTTM-X.
                       20  WRK-INP-CONTR-DTTM PIC  9(009) VALUE ZEROS.
                   15  WRK-INP-NOME-DTTM      PIC  X(030) VALUE SPACES.
                   15  WRK-INP-DTINIC-DTTM    PIC  X(010) VALUE SPACES.
                   15  WRK-INP-DTVENCT-DTTM   PIC  X(010) VALUE SPACES.
                   15  WRK-INP-LIMITE-DTTM    PIC  X(017) VALUE SPACES.
                   15  WRK-INP-SITUAC-DTTM    PIC  X(020) VALUE SPACES.
           05  WRK-INP-MENSA-DTTM       PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                       PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDTTM  - OUTPUT ***'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-DTTM.
           05  WRK-OUT-LL-DTTM          PIC S9(004) COMP VALUE +0739.
           05  WRK-OUT-ZZ-DTTM          PIC  9(004) COMP VALUE ZEROS.
           05  WRK-OUT-SENHAS-DTTM      PIC  X(037) VALUE SPACES.
           05  WRK-OUT-COMANDO-DTTM     PIC  X(068) VALUE SPACES.
           05  WRK-OUT-DADOS-DTTM.
               10  WRK-OUT-TPOOPC-DTTM  PIC  X(001) VALUE SPACES.
               10  WRK-OUT-TMSTAMP-DTTM PIC  X(026) VALUE SPACES.
               10  WRK-OUT-FIMAMOS-DTTM PIC  X(001) VALUE SPACES.
               10  WRK-OUT-PAGINA-DTTM-X.
                   15  WRK-OUT-PAGINA-DTTM
                                        PIC  9(003) VALUE ZEROS.
               10  WRK-OUT-CODAGE-DTTM-X.
                   15  WRK-OUT-CODAGE-DTTM
                                        PIC  9(005) VALUE ZEROS.
               10  WRK-OUT-DESCAGE-DTTM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-TABELA-DTTM.
                   15  WRK-OUT-TABE-DTTM    OCCURS  05  TIMES.
                       20  WRK-OUT-SELEC-ATTR-DTTM PIC  9(04) COMP.
                       20  WRK-OUT-SELEC-DTTM  PIC  X(001) VALUE SPACES.
                       20  WRK-OUT-CONTR-DTTM-X.
                           25  WRK-OUT-CONTR-DTTM
                                               PIC  9(009) VALUE ZEROS.
                       20  WRK-OUT-NOME-DTTM   PIC  X(030) VALUE SPACES.
                       20  WRK-OUT-DTINIC-DTTM PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-DTVENCT-DTTM
                                               PIC  X(010) VALUE SPACES.
                       20  WRK-OUT-LIMITE-DTTM-X.
                           25  WRK-OUT-LIMITE-DTTM
                                               PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                       20  WRK-OUT-SITUAC-DTTM PIC  X(020) VALUE SPACES.
               10  WRK-OUT-MENSA-DTTM   PIC  X(079) VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA TELA DCOMDTTM  - BRAD0660 ***'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMDTTM.
           05  WRK-660-LL-AREA-DTTM     PIC  9(004) COMP   VALUE 0092.
           05  WRK-660-LL-MENSAGEM-DTTM PIC  9(004) COMP   VALUE 0739.
           05  WRK-660-SENHAS-DTTM      PIC  9(004) COMP   VALUE 0037.
           05  WRK-660-COMANDO-DTTM     PIC  9(004) COMP   VALUE 0068.
           05  WRK-660-TPOOPC-DTTM      PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-TMSTAMP-DTTM     PIC  9(004) COMP   VALUE 0026.
           05  WRK-660-FIMAMOS-DTTM     PIC  9(004) COMP   VALUE 0001.
           05  WRK-660-PAGINA-DTTM      PIC  9(004) COMP   VALUE 0003.
           05  WRK-660-CODAGE-DTTM      PIC  9(004) COMP   VALUE 0005.
           05  WRK-660-DESCAGE-DTTM     PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-TABELA-DTTM    OCCURS  05  TIMES.
               10  WRK-660-SELEC-DTTM   PIC  9(004) COMP   VALUE 2003.
               10  WRK-660-CONTR-DTTM   PIC  9(004) COMP   VALUE 0009.
               10  WRK-660-NOME-DTTM    PIC  9(004) COMP   VALUE 0030.
               10  WRK-660-DTINIC-DTTM  PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-DTVENCT-DTTM PIC  9(004) COMP   VALUE 0010.
               10  WRK-660-LIMITE-DTTM  PIC  9(004) COMP   VALUE 0017.
               10  WRK-660-SITUAC-DTTM  PIC  9(004) COMP   VALUE 0020.
           05  WRK-660-MENSA-DTTM       PIC  9(004) COMP   VALUE 0079.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE PASSAGEM DE CONTROLE PARA DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL            PIC S9(004) COMP    VALUE +230.
           05  COMU-ZZ            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO PIC  X(008)         VALUE SPACES.
               10  FILLER         PIC  X(006)         VALUE SPACES.
               10  COMU-PFK       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO       PIC  X(068)         VALUE SPACES.
           05  COMU-TPOOPC        PIC  X(001)         VALUE SPACES.
           05  COMU-TMSTAMP       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO      PIC  9(009)         VALUE ZEROS.
           05  COMU-AGENCIA       PIC  9(005)         VALUE ZEROS.
           05  COMU-CONTA         PIC  9(013)         VALUE ZEROS.
           05  COMU-CPF           PIC  9(009)         VALUE ZEROS.
FS2511*    05  COMU-FILIAL        PIC  9(004)         VALUE ZEROS.
FS2511     05  COMU-FILIAL        PIC  X(04)          VALUE SPACES.
           05  COMU-DIGITO        PIC  9(002)         VALUE ZEROS.
           05  COMU-MENSAG        PIC  X(079)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA DE RECEBIMENTO E RETORNO DO DCOM0413 ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-413.
           05  COMU-LL-413            PIC S9(004) COMP    VALUE +129.
           05  COMU-ZZ-413            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-413.
               10  COMU-TRANSACAO-413 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-413       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-413        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-413       PIC  X(068)         VALUE SPACES.
           05  COMU-TPOOPC-413        PIC  X(001)         VALUE SPACES.
           05  COMU-TMSTAMP-413       PIC  X(026)         VALUE SPACES.
           05  COMU-CODAGE-413        PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM TRANSACAO DCOM0419 **'.
      *----------------------------------------------------------------*
       01  COMU-AREA-422.
           05  COMU-LL-422            PIC S9(004) COMP    VALUE +165.
           05  COMU-ZZ-422            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-422.
               10  COMU-TRANSACAO-422 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(007)         VALUE SPACES.
           05  COMU-SENHAS-422        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-422       PIC  X(068)         VALUE SPACES.
           05  COMU-TPOOPC-422        PIC  X(001)         VALUE SPACES.
           05  COMU-TMSTAMP-422       PIC  X(026)         VALUE SPACES.
           05  COMU-CONTRATO-422      PIC  9(009)         VALUE ZEROS.
           05  COMU-CODAGE-422        PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM6416 **'.
      *----------------------------------------------------------------*

       01  WRK-6416E-ENTRADA.
           03  WRK-6416E-COD-RETORNO      PIC  X(004).
           03  WRK-6416E-MSG-RETORNO      PIC  X(079).
           03  WRK-6416E-RESTART          PIC  9(005).
           03  WRK-6416E-FLAG             PIC  X(001).
           03  WRK-6416E-CFUNC-BDSCO      PIC  9(009).
           03  WRK-6416E-CTERM            PIC  X(008).
           03  WRK-6416E-BANCO            PIC  9(003).
           03  WRK-6416E-DEPDC            PIC  9(005).

       01  WRK-6416S-SAIDA.
           03  WRK-6416S-HEADER.
               05  WRK-6416S-COD-RETORNO  PIC  X(004).
               05  WRK-6416S-MSG-RETORNO  PIC  X(079).
               05  WRK-6416S-RESTART      PIC  9(005).
               05  WRK-6416S-FLAG         PIC  X(001).
           03  WRK-6416S-DADOS.
               05  WRK-6416S-TABELA-SAIDA.
                   07  WRK-6416S-AG-DEPDC            PIC 9(05).
                   07  WRK-6416S-DG-DEPDC            PIC X(01).
                   07  WRK-6416S-NOME-DEPDC          PIC X(40).
                   07  WRK-6416S-END-DEPDC           PIC X(30).
                   07  WRK-6416S-DIRETORIA-REG       PIC 9(05).
                   07  WRK-6416S-DIRETORIA-NOME      PIC X(40).
                   07  WRK-6416S-NOME-BANCO          PIC X(40).
                   07  WRK-6416S-POLO-SERVICO        PIC 9(05).
                   07  WRK-6416S-MUNIC-DEPDC         PIC X(25).
                   07  WRK-6416S-UF                  PIC X(02).
                   07  WRK-6416S-CEP                 PIC 9(05).
                   07  WRK-6416S-CEP-COMPL           PIC 9(03).
                   07  WRK-6416S-INDIC-DEPDC-AT      PIC X(01).
                   07  WRK-6416S-NATUREZA-DEPDC      PIC X(01).
                   07  WRK-6416S-COD-DISTR-CORR      PIC 9(05).
                   07  WRK-6416S-DENCRR-DEPDC-BDSCO  PIC X(10).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO COM O MODULO DCOM4110 **'.
      *----------------------------------------------------------------*
       01  WRK-4110E-ENTRADA.
           03  WRK-4110E-HEADER.
               05  WRK-4110E-COD-RETORNO             PIC  X(04).
               05  WRK-4110E-MSG-RETORNO             PIC  X(79).
               05  WRK-4110E-RESTART                 PIC  9(05).
               05  WRK-4110E-FLAG                    PIC  X(01).
               05  WRK-4110E-START-COUNT             PIC  X(01).
               05  WRK-4110E-CFUNC-BDSCO             PIC  9(09).
               05  WRK-4110E-CTERM                   PIC  X(08).
           03  WRK-4110E-DADOS.
               05  WRK-4110E-CAG-BCRIA               PIC  9(05).
               05  WRK-4110E-FLAG-FILTRO             PIC  X(01).

       01  WRK-4110S-SAIDA.
           03  WRK-4110S-HEADER.
               05  WRK-4110S-COD-RETORNO            PIC  X(04).
               05  WRK-4110S-MSG-RETORNO            PIC  X(79).
               05  WRK-4110S-RESTART                PIC  9(05).
               05  WRK-4110S-FLAG                   PIC  X(01).
               05  WRK-4110S-QTDE-RETORNADA         PIC  9(03).
               05  WRK-4110S-COUNT                  PIC  9(09).
           03  WRK-4110S-DADOS.
               05  WRK-4110S-TABELA-SAIDA    OCCURS 20 TIMES.
                   10  WRK-4110S-CCONTR-LIM-DESC    PIC  9(09).
                   10  WRK-4110S-CVRSAO-CONTR-LIM   PIC  9(03).
FS2511*            10  WRK-4110S-CCNPJ-CPF          PIC  9(09).
FS2511             10  WRK-4110S-CCNPJ-CPF          PIC  X(09).
FS2511*            10  WRK-4110S-CFLIAL-CNPJ        PIC  9(04).
FS2511             10  WRK-4110S-CFLIAL-CNPJ        PIC  X(04).
                   10  WRK-4110S-CCTRL-CNPJ-CPF     PIC  9(02).
                   10  WRK-4110S-NM-CLIE            PIC  X(25).
                   10  WRK-4110S-DINIC-VGCIA-CONTR  PIC  X(10).
                   10  WRK-4110S-DVCTO-CONTR-LIM    PIC  X(10).
                   10  WRK-4110S-VCONTR-LIM         PIC  9(15)V99.
                   10  WRK-4110S-IRSUMO-SIT-DESC    PIC  X(15).
           03  WRK-4110S-CONSISTENCIA   REDEFINES   WRK-4110S-DADOS.
               05  WRK-4110S-TAMANHO-ERRO           PIC  9(01).
               05  WRK-4110S-TABELA-ERRO   OCCURS   1    TIMES.
                   10  WRK-4110S-CAMPOS-ERRO        PIC  9(01).

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA PARA MENSAGENS ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMSY'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DA BRAD7100 ***'.
      *----------------------------------------------------------------*

       COPY 'I#BRAD7C'.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** DCOM0420 - FIM DA AREA DE WORKING ***'.
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

           MOVE SPACES                 TO  WRK-OUT-DADOS-DTTM
                                           WRK-OUT-SENHAS-DTTM
                                           WRK-OUT-COMANDO-DTTM
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

           MOVE 'DCOMDTTM'             TO  WRK-TELA.

           IF  WRK-TRANSACAO    NOT EQUAL  'DCOM0420'
               PERFORM 1100-RECEBER-CONTROLE
           ELSE
               PERFORM 1200-PROCESSAR-DCOMDTTM
           END-IF.

           IF  WRK-TELA             EQUAL  'DCOMDTTM'
               PERFORM 1300-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RECEBE O CONTROLE DE OUTRAS TRANSACOES            *
      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-TRANSACAO  EQUAL  'DCOM0413'
               MOVE WRK-MENSAGEM       TO  COMU-AREA-413
               MOVE COMU-TPOOPC-413    TO  WRK-INP-TPOOPC-DTTM
                                           WRK-OUT-TPOOPC-DTTM
               MOVE COMU-TMSTAMP-413   TO  WRK-INP-TMSTAMP-DTTM
                                           WRK-OUT-TMSTAMP-DTTM
               MOVE COMU-CODAGE-413    TO  WRK-INP-CODAGE-DTTM
                                           WRK-OUT-CODAGE-DTTM
           ELSE
             IF  WRK-TRANSACAO  EQUAL  'DCOM0422'
                 MOVE WRK-MENSAGEM     TO  COMU-AREA-422
                 MOVE COMU-TPOOPC-422  TO  WRK-INP-TPOOPC-DTTM
                                           WRK-OUT-TPOOPC-DTTM
                 MOVE COMU-TMSTAMP-422 TO  WRK-INP-TMSTAMP-DTTM
                                           WRK-OUT-TMSTAMP-DTTM
                 MOVE COMU-CODAGE-422  TO  WRK-INP-CODAGE-DTTM
                                           WRK-OUT-CODAGE-DTTM
             END-IF
           END-IF.

           PERFORM 1110-ACESSAR-DCOM6416

           MOVE ZEROS                  TO  WRK-4110E-RESTART

           PERFORM 1120-ACESSAR-DCOM4110

           MOVE 1                      TO  WRK-INP-PAGINA-DTTM
                                           WRK-OUT-PAGINA-DTTM.

           MOVE 'SELECIONE UM REGISTRO COM "D" E TECLE <ENTER>'
                                    TO  WRK-OUT-MENSA-DTTM.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DCOM6416                          *
      *----------------------------------------------------------------*
       1110-ACESSAR-DCOM6416           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-6416E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-6416E-CTERM
           MOVE 237                    TO  WRK-6416E-BANCO
           MOVE WRK-INP-CODAGE-DTTM    TO  WRK-6416E-DEPDC
           MOVE 'DCOM6416'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-6416E-ENTRADA
                                       WRK-6416S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-6416S-COD-RETORNO
               WHEN '0000'
                    MOVE WRK-6416S-NOME-DEPDC   TO WRK-INP-DESCAGE-DTTM
                                                   WRK-OUT-DESCAGE-DTTM

               WHEN '0003'
                    MOVE  ALL '*'               TO WRK-INP-DESCAGE-DTTM
                                                   WRK-OUT-DESCAGE-DTTM

               WHEN '0099'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    MOVE  WRK-6416S-MSG-RETORNO
                                        TO WRK-OUT-MENSA-DTTM(8:72)
           END-EVALUATE.


      *----------------------------------------------------------------*
       1110-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA ACESSA O MODULO DCOM4110                          *
      *----------------------------------------------------------------*
       1120-ACESSAR-DCOM4110           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-USER           TO  WRK-4110E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO  WRK-4110E-CTERM
           MOVE 'M'                    TO  WRK-4110E-FLAG-FILTRO
           MOVE 'S'                    TO  WRK-4110E-START-COUNT
           MOVE WRK-INP-CODAGE-DTTM    TO  WRK-4110E-CAG-BCRIA
           MOVE 'DCOM4110'             TO  WRK-MODULO

           CALL WRK-MODULO    USING    WRK-4110E-ENTRADA
                                       WRK-4110S-SAIDA
                                       ERRO-AREA
                                       WRK-SQLCA.

           EVALUATE WRK-4110S-COD-RETORNO
               WHEN '0000'
                    IF (WRK-4110S-CCONTR-LIM-DESC(6)  NOT NUMERIC) OR
                       (WRK-4110S-CCONTR-LIM-DESC(6)  EQUAL ZEROS)
                       MOVE '*'     TO  WRK-OUT-FIMAMOS-DTTM
                       MOVE 'FINAL DE AMOSTRAGEM'
                                    TO  WRK-OUT-MENSA-DTTM
                    ELSE
                       MOVE SPACES  TO  WRK-OUT-FIMAMOS-DTTM
                       MOVE 'AMOSTRAGEM CONTINUA'
                                    TO  WRK-OUT-MENSA-DTTM
                    END-IF
                    PERFORM 1121-MONTAR-TELA VARYING WRK-IND FROM
                            1 BY 1 UNTIL WRK-IND GREATER 5

               WHEN '0003'
                    INITIALIZE COMU-AREA
                    MOVE  SG-MENSAGEM(0152)      TO COMU-MENSAG
                    MOVE  WRK-INP-CODAGE-DTTM    TO COMU-AGENCIA
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN '0088'
                    MOVE  'DB2'     TO     ERR-TIPO-ACESSO
                    PERFORM 9999-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                    INITIALIZE COMU-AREA
                    MOVE  WRK-4110S-MSG-RETORNO  TO COMU-MENSAG
                    MOVE  WRK-INP-CODAGE-DTTM    TO COMU-AGENCIA
                    PERFORM 1220-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       1120-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA OS CAMPOS DA TELA                         *
      *----------------------------------------------------------------*
       1121-MONTAR-TELA                SECTION.
      *----------------------------------------------------------------*

           IF (WRK-4110S-CCONTR-LIM-DESC(WRK-IND) IS NUMERIC) AND
              (WRK-4110S-CCONTR-LIM-DESC(WRK-IND) GREATER ZEROS)
              MOVE WRK-4110S-CCONTR-LIM-DESC(WRK-IND)
                                  TO WRK-OUT-CONTR-DTTM(WRK-IND)
              MOVE WRK-4110S-NM-CLIE(WRK-IND)
                                  TO WRK-OUT-NOME-DTTM(WRK-IND)
              MOVE WRK-4110S-DINIC-VGCIA-CONTR(WRK-IND)
                                  TO WRK-OUT-DTINIC-DTTM(WRK-IND)
              INSPECT WRK-OUT-DTINIC-DTTM(WRK-IND) REPLACING ALL
                                                   '.' BY '/'
              MOVE WRK-4110S-DVCTO-CONTR-LIM(WRK-IND)
                                  TO WRK-OUT-DTVENCT-DTTM(WRK-IND)
              INSPECT WRK-OUT-DTVENCT-DTTM(WRK-IND) REPLACING ALL
                                                    '.' BY '/'
              MOVE WRK-4110S-VCONTR-LIM(WRK-IND)
                                  TO WRK-OUT-LIMITE-DTTM(WRK-IND)
              MOVE WRK-4110S-IRSUMO-SIT-DESC(WRK-IND)
                                  TO WRK-OUT-SITUAC-DTTM(WRK-IND)
           ELSE
              MOVE  WRK-241       TO WRK-OUT-SELEC-ATTR-DTTM(WRK-IND)
           END-IF.

      *----------------------------------------------------------------*
       1121-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PROCESSA A MENSAGEM VINDA DA TELA DCOMDTTM        *
      *----------------------------------------------------------------*
       1200-PROCESSAR-DCOMDTTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM             TO WRK-INPUT-DTTM.
           PERFORM 1210-DEVOLVER-TELA

           EVALUATE TRUE
               WHEN WRK-INP-PFK-DTTM     EQUAL 'H' OR
                   (WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL 'PFK1')
                    MOVE WRK-INP-MENSA-DTTM TO WRK-OUT-MENSA-DTTM

               WHEN WRK-INP-PFK-DTTM     EQUAL '3' OR
                   (WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL 'PFK3')
                    INITIALIZE   COMU-AREA
                    PERFORM 1220-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-DTTM     EQUAL 'X' OR
                   (WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL 'PFK5')
                    PERFORM 1230-RETORNAR-MENU-ROTINAS

               WHEN WRK-INP-PFK-DTTM     EQUAL 'Z' OR
                   (WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL 'PFK10')
                    PERFORM 1240-RETORNAR-MENU-DCOM

               WHEN WRK-INP-PFK-DTTM     EQUAL '7' OR
                   (WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL 'PFK7')
                   IF (WRK-INP-CONTR-DTTM(1) NOT NUMERIC) OR
                      (WRK-INP-CONTR-DTTM(1) EQUAL ZEROS)
                      MOVE 'NAO HA DADOS A SEREM MOSTRADOS'
                                              TO WRK-OUT-MENSA-DTTM
                   ELSE
                      PERFORM 1260-TRATAR-VOLTA-PAG
                   END-IF

               WHEN WRK-INP-PFK-DTTM     EQUAL '8' OR
                   (WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL 'PFK8')
                   IF (WRK-INP-CONTR-DTTM(1) NOT NUMERIC) OR
                      (WRK-INP-CONTR-DTTM(1) EQUAL ZEROS)
                      MOVE 'NAO HA DADOS A SEREM MOSTRADOS'
                                              TO WRK-OUT-MENSA-DTTM
                   ELSE
                      PERFORM 1270-TRATAR-AVANCA-PAG
                   END-IF

               WHEN WRK-INP-PFK-DTTM     EQUAL '.' AND
                    WRK-INP-COMANDO-DTTM EQUAL  SPACES OR LOW-VALUES
                    IF (WRK-INP-CONTR-DTTM(1) NOT NUMERIC) OR
                       (WRK-INP-CONTR-DTTM(1) EQUAL ZEROS)
                       MOVE 'NAO HA DADOS A SEREM MOSTRADOS'
                                                 TO WRK-OUT-MENSA-DTTM
                    ELSE
                       PERFORM 1280-TRATAR-ENTER
                    END-IF

               WHEN OTHER
                    MOVE 'PF INVALIDA'   TO WRK-OUT-MENSA-DTTM
           END-EVALUATE.

      *----------------------------------------------------------------*
       1200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA 9EVOLVE OS DADOS DA TELA SEM ALTERACAO            *
      *----------------------------------------------------------------*
       1210-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-INP-FIMAMOS-DTTM   TO WRK-OUT-FIMAMOS-DTTM.
           MOVE WRK-INP-TMSTAMP-DTTM   TO WRK-OUT-TMSTAMP-DTTM.
           MOVE WRK-INP-TPOOPC-DTTM    TO WRK-OUT-TPOOPC-DTTM.
           MOVE WRK-INP-PAGINA-DTTM-X  TO WRK-OUT-PAGINA-DTTM-X.
           MOVE WRK-INP-CODAGE-DTTM-X  TO WRK-OUT-CODAGE-DTTM-X.
           MOVE WRK-INP-DESCAGE-DTTM   TO WRK-OUT-DESCAGE-DTTM.

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
              IF  (WRK-INP-CONTR-DTTM(WRK-IND)  IS NUMERIC) AND
                  (WRK-INP-CONTR-DTTM(WRK-IND)  GREATER ZEROS)
                  MOVE WRK-INP-SELEC-DTTM(WRK-IND)
                                      TO WRK-OUT-SELEC-DTTM(WRK-IND)
                  MOVE WRK-INP-CONTR-DTTM-X(WRK-IND)
                                      TO WRK-OUT-CONTR-DTTM-X(WRK-IND)
                  MOVE WRK-INP-NOME-DTTM(WRK-IND)
                                      TO WRK-OUT-NOME-DTTM(WRK-IND)
                  MOVE WRK-INP-DTINIC-DTTM(WRK-IND)
                                      TO WRK-OUT-DTINIC-DTTM(WRK-IND)
                  MOVE WRK-INP-DTVENCT-DTTM(WRK-IND)
                                      TO WRK-OUT-DTVENCT-DTTM(WRK-IND)
                  MOVE WRK-INP-LIMITE-DTTM(WRK-IND)
                                      TO WRK-OUT-LIMITE-DTTM-X(WRK-IND)
                  MOVE WRK-INP-SITUAC-DTTM(WRK-IND)
                                      TO WRK-OUT-SITUAC-DTTM(WRK-IND)
              ELSE
                   MOVE  WRK-241  TO WRK-OUT-SELEC-ATTR-DTTM(WRK-IND)
              END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA RETORNA AO PROGRAMA CHAMADOR                      *
      *----------------------------------------------------------------*
       1220-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA     TO COMU-LL.
           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0413'           TO WRK-TELA.
           MOVE 'DCOM0420'           TO COMU-TRANSACAO.
           MOVE WRK-INP-TPOOPC-DTTM  TO COMU-TPOOPC.
           MOVE WRK-INP-TMSTAMP-DTTM TO COMU-TMSTAMP.
           MOVE COMU-AREA            TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU ROTINAS    *
      *----------------------------------------------------------------*
       1230-RETORNAR-MENU-ROTINAS      SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA-413 TO COMU-LL-413.
           MOVE WRK-CHNG             TO WRK-FUNCAO.
           MOVE 'DCOM0420'           TO WRK-TELA
                                        COMU-TRANSACAO-413.
           MOVE '5'                  TO COMU-PFK-413.
           MOVE COMU-AREA-413        TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRATAMENTO DE RETORNO AO MENU DCOM       *
      *----------------------------------------------------------------*
       1240-RETORNAR-MENU-DCOM         SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA-413 TO COMU-LL-413.
           MOVE WRK-CHNG           TO WRK-FUNCAO.
           MOVE 'DCOM0420'         TO WRK-TELA
                                      COMU-TRANSACAO-413.
           MOVE 'A'                TO COMU-PFK-413.
           MOVE COMU-AREA-413      TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1240-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA PASSA O CONTROLE PARA TRAN. DCOM0419              *
      *----------------------------------------------------------------*
       1251-CHAMAR-TRAN-DCOM0422       SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH COMU-AREA-422        TO COMU-LL-422.
           MOVE WRK-CHNG                    TO WRK-FUNCAO.
           MOVE 'DCOM0422'                  TO WRK-TELA.
           MOVE 'DCOM0420'                  TO COMU-TRANSACAO-422
           MOVE WRK-INP-SENHAS-DTTM         TO COMU-SENHAS-422
           MOVE WRK-INP-CONTR-DTTM(WRK-IND) TO COMU-CONTRATO-422
           MOVE COMU-AREA-422               TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       1251-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK7 - VOLTA PAG.  *
      *----------------------------------------------------------------*
       1260-TRATAR-VOLTA-PAG           SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-PAGINA-DTTM  EQUAL  1
              MOVE 'NAO EXISTE PAGINA PARA VOLTAR'
                                       TO WRK-OUT-MENSA-DTTM
              GO TO 1260-99-FIM
           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABELA-DTTM
           COMPUTE WRK-4110E-RESTART = (WRK-INP-PAGINA-DTTM - 2) * 5

           PERFORM 1120-ACESSAR-DCOM4110

           COMPUTE WRK-OUT-PAGINA-DTTM = WRK-INP-PAGINA-DTTM - 1.

      *----------------------------------------------------------------*
       1260-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO PFK8 - AVANCA PAG. *
      *----------------------------------------------------------------*
       1270-TRATAR-AVANCA-PAG          SECTION.
      *----------------------------------------------------------------*

           IF WRK-INP-FIMAMOS-DTTM  EQUAL  '*'
              MOVE 'NAO EXISTE PAGINA PARA AVANCAR'
                                       TO WRK-OUT-MENSA-DTTM
              GO TO 1270-99-FIM
           END-IF.

           MOVE  SPACES                TO WRK-OUT-TABELA-DTTM
           COMPUTE WRK-4110E-RESTART = (WRK-INP-PAGINA-DTTM) * 5

           PERFORM 1120-ACESSAR-DCOM4110

           COMPUTE WRK-OUT-PAGINA-DTTM = WRK-INP-PAGINA-DTTM + 1.

      *----------------------------------------------------------------*
       1270-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA EFETUA O TRAMAMENTO DO COMANDO ENTER              *
      *----------------------------------------------------------------*
       1280-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DTTM(WRK-IND) NOT EQUAL 'D' AND SPACES
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DTTM(WRK-IND)
                 MOVE 'OPCAO INVALIDA - SELECIONAR APENAS COM "D"'
                                 TO  WRK-OUT-MENSA-DTTM
             END-IF
           END-PERFORM.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1280-99-FIM
           END-IF.


           MOVE    ZEROS         TO WRK-QTD-OPCAO-D

           PERFORM VARYING WRK-IND FROM 1 BY 1 UNTIL WRK-IND GREATER 5
             IF  WRK-INP-SELEC-DTTM(WRK-IND) EQUAL 'D'
                 ADD  1          TO WRK-QTD-OPCAO-D
             END-IF
           END-PERFORM.

           IF WRK-QTD-OPCAO-D    EQUAL  0
              MOVE '*'         TO WRK-INCONSISTENCIA
              MOVE WRK-49353   TO WRK-OUT-SELEC-ATTR-DTTM(WRK-IND)
              MOVE 'NENHUMA OPCAO FOI SELECIONADA'
                               TO  WRK-OUT-MENSA-DTTM
           ELSE
              IF WRK-QTD-OPCAO-D  GREATER  1
                 MOVE '*'        TO WRK-INCONSISTENCIA
                 MOVE WRK-49353  TO WRK-OUT-SELEC-ATTR-DTTM(WRK-IND)
                 MOVE 'SELECIONAR APENAS UMA OCORRENCIA PARA OPCAO "D"'
                                 TO  WRK-OUT-MENSA-DTTM
              END-IF
           END-IF.

           IF WRK-INCONSISTENCIA  EQUAL  '*'
              GO TO 1280-99-FIM
           END-IF.

           PERFORM VARYING WRK-IND FROM 1 BY 1
                                            UNTIL WRK-IND GREATER 5
                IF WRK-INP-SELEC-DTTM(WRK-IND) EQUAL 'D'
                   PERFORM 1251-CHAMAR-TRAN-DCOM0422
                END-IF
           END-PERFORM.

      *----------------------------------------------------------------*
       1280-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *  ESSA ROTINA CARREGA A TELA QUE SERA DEMONSTRADA PELO IMS.     *
      *----------------------------------------------------------------*
       1300-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL 'BRAD0660'             USING WRK-OUTPUT-DTTM
                                             WRK-660-DCOMDTTM.

           IF  RETURN-CODE             NOT EQUAL ZEROS
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               MOVE RETURN-CODE        TO WRK-RETURN-CODE
               MOVE 10                 TO WRK-LOCAL-ERRO
               MOVE WRK-ERRO-BRAD0660  TO ERR-TEXTO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE WRK-ISRT               TO WRK-FUNCAO.
           MOVE WRK-OUTPUT-DTTM        TO WRK-MENSAGEM.

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
               MOVE 'DCOM0420'         TO ERR-MODULO
           ELSE
               MOVE 'DCOM0420'         TO ERR-PGM
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
