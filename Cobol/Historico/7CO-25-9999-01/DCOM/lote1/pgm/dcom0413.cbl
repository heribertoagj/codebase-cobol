      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*

       PROGRAM-ID.     DCOM0413.
       AUTHOR.         ZEZITO.
      *================================================================*
      *                          A L T R A N                           *
      *----------------------------------------------------------------*
      *     PROGRAMA....: DCOM0413                                     *
      *     PROGRAMADORA: ZEZITO                                       *
      *     ANALISTA....: DANILO                                       *
      *     DATA........: 05/02/2007                                   *
      *----------------------------------------------------------------*
      *     OBJETIVO....: MENU INICIAL OPCAO 04 DO SISTEMA DCOM:       *
      *                   DIRECIONAR PARA A TELA CONFORME OPCAO        *
      *                   SELECIONADA.                                 *
      *----------------------------------------------------------------*
      *     TELAS.......:                                              *
      *     DCOMDMTM....: LIBERAR CONTRATO LIMITE - MENU               *
      *----------------------------------------------------------------*
      *     INC'S.......:                                              *
      *     I#BRAD7C - AREA DE ERRO DA BRAD7100                        *
      *     I#DCOMSG - TABELA DE MENSAGENS DE RETORNO DO               *
      *                SISTEMA DCOM (ON-LINE)                          *
      *     I#DCOMXF - AREA DE COMUNICAO DCOM6438.                     *
      *----------------------------------------------------------------*
      *     MODULOS.....:                                              *
      *     BRAD0660 - INSERIR CARACTERES DE NULL NA MENSAGEM DE SAIDA *
      *     BRAD7100 - TRATAMENTO DE ERROS QUANDO PROGRAMA INVALIDO    *
      *     POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS NO IMS/DC     *
      *     DCOM6438 - VERIFICAR DISPONIBILIDADE DE SISTEMAS.          *
      *----------------------------------------------------------------*
      *     NAVEGACAO...:                                              *
      *     CHAMADO POR.: DCOM0401           CHAMA: DCOM0414           *
      *                                             DCOM0415           *
      *                                             DCOM0416           *
      *                                             DCOM0417           *
      *                                             DCOM0418           *
      *                                             DCOM0419           *
      *                                             DCOM0420           *
      *                                             DCOM0421           *
      *                                             DCOM0422           *
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
       DATA                             DIVISION.
      *================================================================*
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *           *** DCOM0413 - INICIO DA AREA DE WORKING ***         *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *                ***     AREA DE ATIDUTOS ***                    *
      *----------------------------------------------------------------*

      *---  NUMERICO - NORMAL    -    PROTEGIDO  - NAO POS. CURSOR  ---*
       01  WRK-241                     PIC S9(008) COMP  VALUE +241.
      *---  NUMERICO - NORMAL    -    PROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49361                   PIC S9(008) COMP  VALUE +49361.
      *---  NUMERICO - BRILHANTE - DESPROTEGIDO  -     POS. CURSOR  ---*
       01  WRK-49369                   PIC S9(008) COMP  VALUE +49369.
      *--- BRILHANTE, NORMAL    - DEPROTEGIDO  - POS CURSOR - ALFA ---*
       01  WRK-49353                   PIC S9(008) COMP    VALUE +49353.

      *----------------------------------------------------------------*
      *               *** VARIAVEIS AUXILIARES ***                     *
      *----------------------------------------------------------------*

       01  WRK-AUXILIARES.
           05  WRK-MODULO              PIC  X(008)       VALUE SPACES.
           05  WRK-SQLCA               PIC  X(136)       VALUE SPACES.
           05  WRK-INCONSIS            PIC  X(001)       VALUE SPACES.
           05  WRK-QTDE-SELECAO        PIC  9(001)       VALUE ZEROS.

           05  WRK-SENHAS.
               10  WRK-COD-FUNCIONARIO PIC  9(009)       VALUE ZEROS.
               10  WRK-COD-TERMINAL    PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(020)       VALUE SPACES.

           05  WRK-AGENCIA-X.
               10  WRK-AGENCIA-NUM     PIC  9(005)       VALUE ZEROS.

FS2511*01  WRK-CPFCGC.
      *    03  WRK-NUMERO             PIC 9(09).
      *    03  WRK-FILIAL             PIC 9(04).
       01  WRK-CPFCGC.
           03  WRK-NUMERO             PIC X(09).
           03  WRK-FILIAL             PIC X(04).

       01   WRK-CONTROLE               PIC X(02).

       01  WRK-AREA-BRAD0431.
           05  WRK-CONTA-0431          PIC  9(013) COMP-3  VALUE ZEROS.
           05  WRK-TAMANHO-0431        PIC  9(002)         VALUE ZEROS.
           05  WRK-DIGITO-0431         PIC  X(001)         VALUE SPACES.

      *----------------------------------------------------------------*
      *             *** AREAS AUXILIARES PARA POOL5005 ***             *
      *----------------------------------------------------------------*

       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(004)       VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(004)       VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(004)       VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(004)       VALUE SPACES.
           05  WRK-TELA                PIC  X(008)       VALUE SPACES.
           05  WRK-VERSAO              PIC  X(006)       VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(007)       VALUE SPACES.
           05  WRK-COD-USER-R          REDEFINES
               WRK-COD-USER            PIC  9(007).

           05  WRK-COD-DEPTO.
               10  WRK-FILLER          PIC  X(002)       VALUE SPACES.
               10  WRK-COD-DEPTO-N     PIC  9(004)       VALUE ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(004)       VALUE SPACES.
               10  WRK-TRANSACAO       PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(1988)      VALUE SPACES.

      *----------------------------------------------------------------*
      *              *** AREA DE ERRO DA BRAD0660 ***                  *
      *----------------------------------------------------------------*

       01  WRK-ERRO-BRAD0660.
           05  FILLER                  PIC  X(045)       VALUE
               '* RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05  FILLER                  PIC  X(012)       VALUE
               '- RET.CODE ='.
           05  WRK-RETURN-CODE         PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(011)       VALUE
               ' - LOCAL ='.
           05  WRK-LOCAL-ERRO          PIC  9(002)       VALUE ZEROS.
           05  FILLER                  PIC  X(002)       VALUE ' *'.

FS2511 01 WRK-BRAD2000                 PIC  X(08)  VALUE 'BRAD2000'.
FS2511 01 WRK-AREA-BRAD2000.
FS2511    05 WRK-CAD-CPFCGC.
FS2511       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
FS2511       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.
FS2511    05 WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.
      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOM0413  -  INPUT ***                 *
      *----------------------------------------------------------------*

       01  INPUT-DMTM.
           05  FILLER                    PIC  X(018)     VALUE SPACES.
           05  INP-PFK                   PIC  X(001)     VALUE SPACES.
           05  INP-SENHAS                PIC  X(037)     VALUE SPACES.
           05  INP-COMANDO               PIC  X(068)     VALUE SPACES.
           05  INP-DADOS.
               10  INP-TPOOPC.
                   15  INP-TPOOPC-N      PIC  9(001)     VALUE ZEROS.
               10  INP-TMSTAMP           PIC  X(026)     VALUE SPACES.
               10  INP-OPCAO.
                   15  INP-OPCAO-N       PIC  9(001)     VALUE ZEROS.
               10  INP-OPER.
                   15  INP-OPER-N        PIC  9(013)     VALUE ZEROS.
               10  INP-AGENCIA.
                   15  INP-AGENCIA-N     PIC  9(005)     VALUE ZEROS.
               10  INP-AGENC.
                   15  INP-AGENC-N       PIC  9(005)     VALUE ZEROS.
               10  INP-CONTA.
                   15  INP-CONTA-N       PIC  9(013)     VALUE ZEROS.
FS2511*    15  INP-CNPJ-N        PIC  9(009)     VALUE ZEROS.
               10  INP-CNPJ.
                   15  INP-CNPJ-N        PIC  X(009)     VALUE SPACES.
FS2511*    15  INP-FILIAL-N      PIC  9(004)     VALUE ZEROS.
               10  INP-FILIAL.
                   15  INP-FILIAL-N      PIC  X(004)     VALUE SPACES.
               10  INP-DIG.
                   15  INP-DIG-N         PIC  9(002)     VALUE ZEROS.
           05  INP-MENSAG                PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOM0413  -  OUTPUT ***                *
      *----------------------------------------------------------------*

       01  OUTPUT-DMTM.
           05  OUT-LL                    PIC S9(004) COMP VALUE +0283.
           05  OUT-ZZ                    PIC S9(004) COMP VALUE +0.
           05  OUT-SENHAS                PIC  X(037)      VALUE SPACES.
           05  OUT-COMANDO               PIC  X(068)      VALUE SPACES.
           05  OUT-DADOS.
               10  OUT-TPOOPC.
                   15  OUT-TPOOPC-N      PIC  9(001)     VALUE ZEROS.
               10  OUT-TMSTAMP           PIC  X(026)     VALUE SPACES.
               10  OUT-OPCAO-ATTR        PIC S9(004) COMP VALUE  +0.
               10  OUT-OPCAO.
                   15  OUT-OPCAO-N       PIC  9(001)     VALUE ZEROS.
               10  OUT-OPER-ATTR         PIC S9(004) COMP VALUE  +0.
               10  OUT-OPER.
                   15  OUT-OPER-N        PIC  9(013)     VALUE ZEROS.
               10  OUT-AGENCIA-ATTR      PIC S9(004) COMP VALUE  +0.
               10  OUT-AGENCIA.
                   15  OUT-AGENCIA-N     PIC  9(005)     VALUE ZEROS.
               10  OUT-AGENC-ATTR        PIC S9(004) COMP VALUE  +0.
               10  OUT-AGENC.
                   15  OUT-AGENC-N       PIC  9(005)     VALUE ZEROS.
               10  OUT-CONTA-ATTR        PIC S9(004) COMP VALUE  +0.
               10  OUT-CONTA.
                   15  OUT-CONTA-N       PIC  9(013)     VALUE ZEROS.
               10  OUT-CNPJ-ATTR         PIC S9(004) COMP VALUE  +0.
FS2511*            15  OUT-CNPJ-N        PIC  9(009)     VALUE ZEROS.
                   15  OUT-CNPJ-N        PIC  X(009)     VALUE SPACES.
               10  OUT-FILIAL-ATTR       PIC S9(004) COMP VALUE  +0.
               10  OUT-FILIAL.
 FS2511*           15  OUT-FILIAL-N      PIC  9(004)     VALUE ZEROS.
                   15  OUT-FILIAL-N      PIC  X(004)     VALUE SPACES.
               10  OUT-DIG-ATTR          PIC S9(004) COMP VALUE  +0.
               10  OUT-DIG.
                   15  OUT-DIG-N         PIC  9(002)     VALUE ZEROS.
           05  OUT-MENSAG                PIC  X(079)     VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DA TELA DCOMDMTM - BRAD0660 ***                *
      *----------------------------------------------------------------*

       01  660-DCOMDMTM.
           05  660-LL-AREA-DMTM          PIC  9(004) COMP  VALUE 0030.
           05  660-LL-MENSAGEM-DMTM      PIC  9(004) COMP  VALUE 0283.
           05  660-SENHAS-DMTM           PIC  9(004) COMP  VALUE 0037.
           05  660-COMANDO-DMTM          PIC  9(004) COMP  VALUE 0068.
           05  660-TPOOPC-DMTM           PIC  9(004) COMP  VALUE 0001.
           05  660-TMSTAMP-DMTM          PIC  9(004) COMP  VALUE 0026.
           05  660-OPCAO-DMTM            PIC  9(004) COMP  VALUE 2003.
           05  660-OPER-DMTM             PIC  9(004) COMP  VALUE 2015.
           05  660-AGENCIA-DMTM          PIC  9(004) COMP  VALUE 2007.
           05  660-AGENC-DMTM            PIC  9(004) COMP  VALUE 2007.
           05  660-CONTA-DMTM            PIC  9(004) COMP  VALUE 2015.
           05  660-CNPJ-DMTM             PIC  9(004) COMP  VALUE 2011.
           05  660-FILIAL-DMTM           PIC  9(004) COMP  VALUE 2006.
           05  660-DIG-DMTM              PIC  9(004) COMP  VALUE 2004.
           05  660-MENSA-DMTM            PIC  9(004) COMP  VALUE 0079.

      *----------------------------------------------------------------*
      *                *** AREA DE COMUNICACAO ***                     *
      *----------------------------------------------------------------*

       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP  VALUE +272.
           05  COMU-ZZ                 PIC S9(004) COMP  VALUE +0.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)       VALUE SPACES.
               10  FILLER              PIC  X(006)       VALUE SPACES.
               10  COMU-PFK            PIC  X(001)       VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)       VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)       VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-OPCAO          PIC  9(001)       VALUE ZEROS.
               10  COMU-TIMESTAMP      PIC  X(026)       VALUE SPACES.
               10  COMU-CONTRATO       PIC  9(009)       VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)       VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)       VALUE ZEROS.
FS2511*        10  COMU-CPF            PIC  9(009)       VALUE ZEROS.
               10  COMU-CPF            PIC  X(009)       VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)       VALUE ZEROS.
               10  COMU-FILIAL         PIC  X(004)       VALUE SPACES.
               10  COMU-DIGITO         PIC  9(002)       VALUE ZEROS.
               10  COMU-MENSAG         PIC  X(079)       VALUE SPACES.

      *----------------------------------------------------------------*
      *        *** AREA DE COMUNICACAO PFK ***                         *
      *----------------------------------------------------------------*

       01  COMU-AREA-PFK.
           05  COMU-LL-PFK             PIC S9(004) COMP    VALUE +124.
           05  COMU-ZZ-PFK             PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-PFK  PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK1           PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-PFK         PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-PFK        PIC  X(068)         VALUE SPACES.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA RECEBIMENTO E RETORNO DO DCOM0414/0417/0420   ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-414.
           05  COMU-LL-414            PIC S9(004) COMP    VALUE +155.
           05  COMU-ZZ-414            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-414.
               10  COMU-TRANSACAO-414 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-414       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-414        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-414       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-414.
               10  COMU-OPCAO-414     PIC  9(001)         VALUE ZEROS.
               10  COMU-TMSTAMP-414   PIC  X(026)         VALUE SPACES.
               10  COMU-CODAGE-414    PIC  9(005)         VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA RECEBIMENTO E RETORNO DO DCOM0415/0415/0421   ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA-415.
           05  COMU-LL-415            PIC S9(004) COMP    VALUE +183.
           05  COMU-ZZ-415            PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE-415.
               10  COMU-TRANSACAO-415 PIC  X(008)         VALUE SPACES.
               10  FILLER             PIC  X(006)         VALUE SPACES.
               10  COMU-PFK-415       PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS-415        PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO-415       PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS-415.
               10  COMU-OPCAO-415     PIC  9(001)         VALUE ZEROS.
               10  COMU-TMSTAMP-415   PIC  X(026)         VALUE SPACES.
               10  COMU-CODAGE-415    PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA-415     PIC  9(013)         VALUE ZEROS.
FS2511*        10  COMU-NUMERO-415    PIC  9(009)         VALUE ZEROS.
               10  COMU-NUMERO-415    PIC  X(009)         VALUE SPACES.
FS2511*        10  COMU-FILIAL-415    PIC  9(004)         VALUE ZEROS.
               10  COMU-FILIAL-415    PIC  X(004)         VALUE SPACES.
               10  COMU-CONTROLE-415  PIC  9(002)         VALUE ZEROS.

      *---------------------------------------------------------------
       01  FILLER                      PIC  X(080)         VALUE
           '*** AREA RECEBIMENTO E RETORNO DO DCOM0416/0419/0422   ***'.
      *---------------------------------------------------------------

       01  COMU-AREA-416.
           05  COMU-LL-416             PIC S9(004) COMP VALUE +160.
           05  COMU-ZZ-416             PIC S9(004) COMP VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO-416  PIC  X(008)    VALUE SPACES.
               10  FILLER              PIC  X(007)    VALUE SPACES.
           05  COMU-SENHAS-416         PIC  X(037)    VALUE SPACES.
           05  COMU-COMANDO-416        PIC  X(068)    VALUE SPACES.
           05  COMU-DADOS-416.
               10  COMU-OPCAO-416      PIC  9(001)    VALUE ZEROS.
               10  COMU-TMSTAMP-416    PIC  X(026)    VALUE SPACES.
               10  COMU-OPERACAO-416   PIC  9(009)    VALUE ZEROS.

      *----------------------------------------------------------------*
       01  FILLER                        PIC  X(050)         VALUE
           '*** AREA DO DCOM4128 ***'.
      *----------------------------------------------------------------*

       01  ENTRADA-4128.
           03  RE4128-COD-RETORNO        PIC  X(04).
           03  RE4128-MSG-RETORNO        PIC  X(79).
           03  RE4128-RESTART            PIC  9(05).
           03  RE4128-FLAG               PIC  X(01).
           03  RE4128-CFUNC-BDSCO        PIC  9(09).
           03  RE4128-CTERM              PIC  X(08).
           03  RE4128-CCONTR-LIM-DESC    PIC  9(09).
           03  RE4128-VERSAO-LIM-DESC    PIC  9(03).
           03  RE4128-FLAG-FILTRO        PIC  X(01).

       01  SAIDA-4128.
           03  RS4128-HEADER.
               05  RS4128-COD-RETORNO              PIC  X(04).
               05  RS4128-MSG-RETORNO              PIC  X(79).
               05  RS4128-RESTART                  PIC  9(05).
               05  RS4128-FLAG                     PIC  X(01).
           03  RS4128-DADOS.
               05  RS4128-CPRODT                   PIC  9(03).
               05  RS4128-ISIT-DESC-COML           PIC  X(30).
               05  RS4128-IRSUMO-SIT-DESC          PIC  X(15).
FS2511*        05  RS4128-CCNPJ-CPF                PIC  9(09).
               05  RS4128-CCNPJ-CPF                PIC  X(09).
FS2511*        05  RS4128-CFLIAL-CNPJ              PIC  9(04).
               05  RS4128-CFLIAL-CNPJ              PIC  X(04).
               05  RS4128-CCTRL-CNPJ               PIC  9(02).
               05  RS4128-IPSSOA-DESC-COML         PIC  X(60).
               05  RS4128-CBCO                     PIC  9(03).
               05  RS4128-CBCO-DESC                PIC  X(20).
               05  RS4128-CAG-BCRIA                PIC  9(05).
               05  RS4128-CAG-BCRIA-DESC           PIC  X(30).
               05  RS4128-CCTA-BCRIA-CLI           PIC  9(13).
               05  RS4128-DINIC-VGCIA-CONTR        PIC  X(10).
               05  RS4128-CINDCD-RENOV-AUTOM       PIC  X(01).
               05  RS4128-DVCTO-CONTR-LIM          PIC  X(10).
               05  RS4128-TCONTR-LIM-DESC          PIC  9(05).
               05  RS4128-VCONTR-LIM-DESC          PIC  9(15)V99.
               05  RS4128-VDISPN-CONTR-LIM         PIC  9(15)V99.
               05  RS4128-CTPO-COBR-TAC            PIC  9(03).
               05  RS4128-ITPO-COBR-TAC            PIC  X(30).
               05  RS4128-VTAC-CONTR-LIM           PIC  9(15)V99.
               05  RS4128-PARECER                  PIC  X(240).
               05  RS4128-CGARAN                   PIC  9(003).
               05  RS4128-IGARAN                   PIC  X(040).
               05  RS4128-VUTILZ-LIM-DESC          PIC  9(15)V99.
           03  RS4128-CONSISTENCIA REDEFINES RS4128-DADOS.
               05  RS4128-TAMANHO-ERRO             PIC  9(01).
               05  RS4128-TABELA-ERROS             OCCURS 01 TIMES.
                   07  RS4128-CAMPOS-ERRO          PIC  9(01).

      *----------------------------------------------------------------*
      *        *** AREA DA BRAD7100 ***                                *
      *----------------------------------------------------------------*

       COPY I#BRAD7C.

      *----------------------------------------------------------------*
      *        *** AREA DA MENSAGENS DE ERRO                           *
      *----------------------------------------------------------------*

       COPY I#DCOMSY.

      *----------------------------------------------------------------*
      *        *** AREA DO PROGRAMA DCOM6408 ***                       *
      *----------------------------------------------------------------*

       COPY I#DCOMX3.

      *----------------------------------------------------------------*
      *        *** AREA DE ACESSO AO MODULO DCOM6437                   *
      *----------------------------------------------------------------*

       COPY I#DCOMXE.

      *----------------------------------------------------------------*
      *        *** AREA DO PROGRAMA DCOM6438 ***                       *
      *----------------------------------------------------------------*

       COPY I#DCOMXF.

      *----------------------------------------------------------------*
      *        *** DCOM0413 - FIM DA AREA DE WORKING ***               *
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*

       01  LNK-IO-PCB.
           05  LNK-IO-LTERM            PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-IO-STATUS           PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-IO-MODNAME          PIC  X(008).

       01  LNK-ALT-PCB.
           05  LNK-ALT-LTERM           PIC  X(008).
           05  FILLER                  PIC  X(002).
           05  LNK-ALT-STATUS          PIC  X(002).
           05  FILLER                  PIC  X(012).
           05  LNK-ALT-MODNAME         PIC  X(008).


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING  LNK-IO-PCB
                                              LNK-ALT-PCB.

           MOVE  WRK-GU                TO  WRK-FUNCAO.

      *----------------------------------------------------------------*
      *  ESSA ROTINA CONTEM OS PROCEDIMENTOS INICIAIS                  *
      *----------------------------------------------------------------*
       0000-INICIAR.
      *----------------------------------------------------------------*

           CALL  'POOL5005'            USING  WRK-VERSAO
                                              WRK-FUNCAO
                                              LNK-IO-PCB
                                              LNK-ALT-PCB
                                              WRK-MENSAGEM
                                              WRK-TELA
                                              WRK-COD-USER
                                              WRK-COD-DEPTO.

           IF  RETURN-CODE          EQUAL  04 OR 12
               GOBACK
           END-IF.

           MOVE SPACES                 TO  OUT-DADOS
                                           OUT-SENHAS
                                           OUT-COMANDO.

           INITIALIZE                  WRK-AUXILIARES
                                       COMU-AREA
                                       COMU-AREA-414
                                       COMU-AREA-415
                                       COMU-AREA-416.



           PERFORM 1000-PROCESSAR-MENSAGEM.

           GO TO 0000-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1000-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE  'DCOMDMTM'            TO  WRK-TELA.

           IF WRK-TRANSACAO     NOT EQUAL  'DCOM0413'
              PERFORM 1100-RECEBER-CONTROLE
              IF  WRK-TRANSACAO     EQUAL  'DCOM0401'  OR
                  COMU-MENSAG       EQUAL  SPACES
                  MOVE SG-MENSAGEM(0014)    TO  OUT-MENSAG
              END-IF
           ELSE
              PERFORM  2000-PROCESSAR-DCOMDMTM
           END-IF.

           IF WRK-TELA              EQUAL  'DCOMDMTM'
              PERFORM  3000-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                   EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       1100-RECEBER-CONTROLE           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  COMU-AREA.

           MOVE COMU-SENHAS            TO OUT-SENHAS.

           MOVE 2                      TO OUT-TPOOPC-N.
           MOVE COMU-TIMESTAMP         TO OUT-TMSTAMP.
           MOVE COMU-MENSAG            TO OUT-MENSAG.

           EVALUATE     COMU-TRANSACAO
              WHEN 'DCOM0416'
              WHEN 'DCOM0419'
              WHEN 'DCOM0422'
                    IF  COMU-CONTRATO  GREATER ZEROS
                        MOVE COMU-OPCAO        TO  OUT-OPCAO-N
                        MOVE COMU-CONTRATO     TO  OUT-OPER-N
                        MOVE WRK-49369         TO  OUT-OPER-ATTR
                    END-IF

              WHEN 'DCOM0415'
              WHEN 'DCOM0418'
FS2511*       WHEN 'DCOM0421'
FS2511*             IF  COMU-CPF               GREATER ZEROS AND
FS2511*                 COMU-CPF               IS  NUMERIC
FS2511        WHEN 'DCOM0421'
FS2511            IF  COMU-CPF           NOT EQUAL SPACES AND
FS2511                                   NOT EQUAL LOW-VALUES
                        MOVE COMU-OPCAO        TO  OUT-OPCAO-N
                        MOVE COMU-CPF          TO  OUT-CNPJ-N
                        MOVE COMU-FILIAL       TO  OUT-FILIAL-N
                        MOVE COMU-DIGITO       TO  OUT-DIG-N
                        MOVE WRK-49369         TO  OUT-CNPJ-ATTR
                                                   OUT-FILIAL-ATTR
                                                   OUT-DIG-ATTR
                    ELSE
                    IF  COMU-CONTA          IS NUMERIC AND
                        COMU-CONTA             GREATER ZEROS
                        MOVE COMU-OPCAO        TO  OUT-OPCAO-N
                        MOVE COMU-CONTA        TO  OUT-CONTA-N
                        MOVE COMU-AGENCIA      TO  OUT-AGENC-N
                        MOVE WRK-49369         TO  OUT-AGENC-ATTR
                                                   OUT-CONTA-ATTR
                    END-IF
                    END-IF

           END-EVALUATE.

      *----------------------------------------------------------------*
       1100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2000-PROCESSAR-DCOMDMTM         SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-MENSAGEM           TO  INPUT-DMTM.

           PERFORM  2100-DEVOLVER-TELA

           EVALUATE  TRUE
               WHEN  INP-PFK          EQUAL  'H'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK01')
                     MOVE  INP-MENSAG     TO  OUT-MENSAG

               WHEN  INP-PFK          EQUAL  '3'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL 'PFK03')
                     PERFORM  2200-RETORNAR-CHAMADOR

               WHEN  INP-PFK          EQUAL  '5'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK05')
                     PERFORM  2300-MENU-ROTINAS

               WHEN  INP-PFK          EQUAL  'A'  OR
                    (INP-PFK          EQUAL  '.'  AND
                     INP-COMANDO      EQUAL  'PFK10')
                     PERFORM  2400-MENU-DCOM

               WHEN  INP-PFK          EQUAL '.' AND
                     INP-COMANDO      EQUAL  SPACES
                     PERFORM  2500-TRATAR-ENTER
                     PERFORM  2100-DEVOLVER-TELA

               WHEN  OTHER
                     MOVE  SG-MENSAGEM(0038)      TO OUT-MENSAG
           END-EVALUATE.

      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2100-DEVOLVER-TELA              SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER                 NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-OPER-N      REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           IF INP-AGENCIA              NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-AGENCIA-N   REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           IF INP-AGENC                NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-AGENC-N     REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           IF INP-CONTA                NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-CONTA-N     REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           IF INP-CNPJ                 NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-CNPJ-N      REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           IF INP-FILIAL               NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-FILIAL-N    REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           IF INP-DIG                  NOT EQUAL SPACES AND LOW-VALUES
              INSPECT  INP-DIG-N       REPLACING  LEADING  ' '  BY  '0'
           END-IF.

           MOVE  INP-TPOOPC            TO  OUT-TPOOPC
           MOVE  INP-TMSTAMP           TO  OUT-TMSTAMP
           MOVE  INP-OPCAO             TO  OUT-OPCAO
           MOVE  INP-OPER              TO  OUT-OPER
           MOVE  INP-AGENCIA           TO  OUT-AGENCIA
           MOVE  INP-AGENC             TO  OUT-AGENC
           MOVE  INP-CONTA             TO  OUT-CONTA
           MOVE  INP-CNPJ              TO  OUT-CNPJ
           MOVE  INP-FILIAL            TO  OUT-FILIAL
           MOVE  INP-DIG               TO  OUT-DIG.

      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2200-RETORNAR-CHAMADOR          SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     COMU-AREA.

           MOVE LENGTH COMU-AREA       TO COMU-LL.
           MOVE INP-SENHAS             TO COMU-SENHAS.
           MOVE INP-OPCAO-N            TO COMU-OPCAO.
           MOVE INP-TMSTAMP            TO COMU-TIMESTAMP
           MOVE WRK-CHNG               TO WRK-FUNCAO
           MOVE 'DCOM0401'             TO WRK-TELA
           MOVE 'DCOM0413'             TO COMU-TRANSACAO
           MOVE COMU-AREA              TO WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *     ACESSAR DCOM4128                                           *
      *----------------------------------------------------------------*
       2273-ACESSAR-DCOM4128           SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                    TO RE4128-RESTART
           MOVE SPACES                   TO RE4128-FLAG
           MOVE WRK-COD-USER             TO RE4128-CFUNC-BDSCO
           MOVE LNK-IO-LTERM             TO RE4128-CTERM
           MOVE INP-OPER-N               TO RE4128-CCONTR-LIM-DESC

           EVALUATE INP-OPCAO
               WHEN '1'
                    MOVE 'L'             TO RE4128-FLAG-FILTRO

               WHEN '2'
                    MOVE 'M'             TO RE4128-FLAG-FILTRO

               WHEN '3'
                    MOVE 'H'             TO RE4128-FLAG-FILTRO
           END-EVALUATE

           MOVE 'DCOM4128'               TO WRK-MODULO.
           CALL WRK-MODULO               USING ENTRADA-4128
                                               SAIDA-4128
                                               ERRO-AREA
                                               WRK-SQLCA.

           IF  RS4128-COD-RETORNO              EQUAL '0003' OR
                                                     '0100'
               MOVE SG-MENSAGEM(0134)     TO OUT-MENSAG
               MOVE WRK-49369             TO OUT-OPER-ATTR
               MOVE '*'                   TO WRK-INCONSIS
           ELSE
               IF RS4128-COD-RETORNO      NOT EQUAL '0000'
                  MOVE RS4128-MSG-RETORNO
                                          TO OUT-MENSAG
                  MOVE WRK-49369          TO OUT-OPER-ATTR
                  MOVE '*'                TO WRK-INCONSIS
                  PERFORM 2299-VALIDAR-PERFIL
               ELSE
                  PERFORM 2299-VALIDAR-PERFIL
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2273-99-FIM.                  EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2299-VALIDAR-PERFIL             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6437-ENVIO
                      ROTSAI-6437-RETORNO.

           MOVE WRK-COD-USER           TO ROTENT-6437-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6437-CTERM.
           MOVE WRK-COD-DEPTO-N        TO WRK-AGENCIA-NUM
           MOVE WRK-AGENCIA-NUM        TO ROTENT-6437-AGEUSU-ENV.

           IF  INP-AGENCIA            NOT EQUAL SPACES
               MOVE INP-AGENCIA-N     TO  ROTENT-6437-AGEPESQ-ENV
           ELSE
             IF  INP-AGENC-N            NOT EQUAL SPACES
                 MOVE INP-AGENC-N       TO  ROTENT-6437-AGEPESQ-ENV
             ELSE
                 MOVE RS4128-CAG-BCRIA  TO  ROTENT-6437-AGEPESQ-ENV
           END-IF.

           MOVE 'A'                    TO ROTENT-6437-FUNCAO-ENV.
           MOVE 'DCOM6437'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6437-ENVIO
                                             ROTSAI-6437-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6437-COD-RET     NOT EQUAL ZEROS OR
               ROTSAI-6437-RESUL-RET   GREATER 3
               MOVE '*'                TO WRK-INCONSIS
               MOVE SG-MENSAGEM(0204)  TO OUT-MENSAG
               IF   INP-AGENCIA        NOT EQUAL SPACES
                    MOVE WRK-49369     TO OUT-AGENCIA-ATTR
               ELSE
                    IF   INP-AGENC-N     NOT EQUAL SPACES
                         MOVE WRK-49369      TO  OUT-CONTA-ATTR
                                                 OUT-AGENC-ATTR
                    ELSE
                         MOVE WRK-49369      TO  OUT-OPER-ATTR
                    END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2299-99-FIM.                    EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
       2300-MENU-ROTINAS               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.
           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0413'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  '5'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2400-MENU-DCOM                  SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                      COMU-AREA-PFK.

           MOVE  +124                  TO  COMU-LL-PFK.
           MOVE  ZEROS                 TO  COMU-ZZ-PFK.
           MOVE  WRK-CHNG              TO  WRK-FUNCAO.
           MOVE  'DCOM0413'            TO  WRK-TELA
                                           COMU-TRANSACAO-PFK.
           MOVE  'A'                   TO  COMU-PFK1.
           MOVE  COMU-AREA-PFK         TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2500-TRATAR-ENTER               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO WRK-INCONSIS

           PERFORM 2510-CONSISTENCIA-FISICA.

           IF  WRK-INCONSIS            EQUAL SPACES
               PERFORM 2580-VER-DISPONIBILIDADE

               IF  WRK-INCONSIS        EQUAL SPACES
                   EVALUATE INP-OPCAO
                       WHEN '1'
                            PERFORM 2520-TRATAR-LIBERAR

                       WHEN '2'
                            PERFORM 2530-TRATAR-CANCELAR

                       WHEN '3'
                            PERFORM 2540-TRATAR-CONSULTA

                       WHEN OTHER
                            MOVE WRK-49369          TO OUT-OPCAO-ATTR
                            MOVE SG-MENSAGEM(0035)  TO OUT-MENSAG
                   END-EVALUATE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       2500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2510-CONSISTENCIA-FISICA        SECTION.
      *----------------------------------------------------------------*

           IF  INP-OPCAO      NOT EQUAL  SPACES
               IF (INP-OPCAO-N NOT NUMERIC)         OR
                  (INP-OPCAO-N  EQUAL ZEROS)        OR
                  (INP-OPCAO  NOT EQUAL 1 AND 2 AND 3)
                  MOVE SG-MENSAGEM (35)   TO  OUT-MENSAG
                  MOVE '*'                TO  WRK-INCONSIS
                  MOVE WRK-49369          TO  OUT-OPCAO-ATTR
                  GO                      TO  2510-99-FIM
               END-IF
           ELSE
               MOVE SG-MENSAGEM(21)       TO  OUT-MENSAG
               MOVE '*'                   TO  WRK-INCONSIS
               GO TO 2510-99-FIM
           END-IF.

           IF  (INP-OPER           EQUAL  SPACES OR LOW-VALUES) AND
               (INP-AGENCIA        EQUAL  SPACES OR LOW-VALUES) AND
               (INP-AGENC          EQUAL  SPACES OR LOW-VALUES) AND
               (INP-CONTA          EQUAL  SPACES OR LOW-VALUES) AND
               (INP-CNPJ           EQUAL  SPACES OR LOW-VALUES) AND
               (INP-DIG            EQUAL  SPACES OR LOW-VALUES)
                MOVE WRK-49369            TO  OUT-OPER-ATTR
                                              OUT-AGENCIA-ATTR
                                              OUT-AGENC-ATTR
                                              OUT-CONTA-ATTR
                                              OUT-CNPJ-ATTR
                                              OUT-FILIAL-ATTR
                                              OUT-DIG-ATTR
                MOVE 'ENTRE COM UMA DAS COMBINACOES: CONTRATO OU AGENCIA
      -              ' OU AGENCIA/CONTA OU CNPJ/CPF'
                                          TO  OUT-MENSAG
                MOVE '*'                  TO  WRK-INCONSIS
                GO                        TO  2510-99-FIM
           END-IF.

           MOVE  ZEROS                    TO  WRK-QTDE-SELECAO.

           IF  (INP-OPER       NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
                MOVE WRK-49369            TO  OUT-OPER-ATTR
           END-IF.

           IF  (INP-AGENCIA    NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
                MOVE WRK-49369            TO  OUT-AGENCIA-ATTR
           END-IF.

           IF  (INP-AGENC      NOT EQUAL  SPACES AND LOW-VALUES)
           OR  (INP-CONTA      NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
                MOVE WRK-49369            TO  OUT-AGENC-ATTR
                                              OUT-CONTA-ATTR
           END-IF.

           IF  (INP-CNPJ       NOT EQUAL  SPACES AND LOW-VALUES) OR
               (INP-DIG        NOT EQUAL  SPACES AND LOW-VALUES)
                ADD  1                    TO  WRK-QTDE-SELECAO
                MOVE WRK-49369            TO  OUT-CNPJ-ATTR
                                              OUT-FILIAL-ATTR
                                              OUT-DIG-ATTR
           END-IF.

           IF  (WRK-QTDE-SELECAO       EQUAL  ZEROS  OR
                                       GREATER  1)
                MOVE 'ENTRE COM UMA DAS COMBINACOES: CONTRATO OU AGENCIA
      -              ' OU AGENCIA/CONTA OU CNPJ/CPF'
                                       TO  OUT-MENSAG
                MOVE '*'               TO  WRK-INCONSIS
                GO                     TO  2510-99-FIM
           END-IF.

           IF  INP-OPER                NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-OPER-N          NOT NUMERIC)  OR
                  (INP-OPER-N          EQUAL ZEROS)
                  MOVE SG-MENSAGEM (133)  TO  OUT-MENSAG
                  MOVE '*'             TO  WRK-INCONSIS
                  MOVE WRK-49369       TO  OUT-OPER-ATTR
                  GO                   TO  2510-99-FIM
               ELSE
                  PERFORM 2273-ACESSAR-DCOM4128
                  IF  WRK-INCONSIS     EQUAL '*'
                      GO               TO  2510-99-FIM
                  END-IF
               END-IF
           END-IF.

           IF  INP-AGENCIA             NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-AGENCIA-N       NOT NUMERIC)  OR
                  (INP-AGENCIA-N       EQUAL ZEROS)
                  MOVE SG-MENSAGEM (57)   TO  OUT-MENSAG
                  MOVE '*'             TO  WRK-INCONSIS
                  MOVE WRK-49369       TO  OUT-AGENCIA-ATTR
                  GO                   TO  2510-99-FIM
               END-IF
           END-IF.

           IF  INP-AGENC               NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-AGENC-N         NOT NUMERIC)  OR
                  (INP-AGENC-N         EQUAL ZEROS)
                  MOVE SG-MENSAGEM (57)   TO  OUT-MENSAG
                  MOVE '*'             TO  WRK-INCONSIS
                  MOVE WRK-49369       TO  OUT-AGENC-ATTR
                  GO                   TO  2510-99-FIM
               END-IF
           ELSE
               IF  INP-CONTA           NOT EQUAL SPACES AND  LOW-VALUES
                   MOVE SG-MENSAGEM (57)   TO  OUT-MENSAG
                   MOVE '*'            TO  WRK-INCONSIS
                   MOVE WRK-49369      TO  OUT-AGENC-ATTR
                   GO                  TO  2510-99-FIM
               END-IF
           END-IF.

           IF  INP-CONTA               NOT EQUAL SPACES AND  LOW-VALUES
               IF (INP-CONTA-N         NOT NUMERIC)  OR
                  (INP-CONTA-N         EQUAL ZEROS)
                  MOVE SG-MENSAGEM (58)   TO  OUT-MENSAG
                  MOVE '*'             TO  WRK-INCONSIS
                  MOVE WRK-49369       TO  OUT-CONTA-ATTR
                  GO                   TO  2510-99-FIM
               END-IF
           ELSE
               IF  INP-AGENC           NOT EQUAL SPACES AND LOW-VALUES
                   MOVE SG-MENSAGEM (58)   TO  OUT-MENSAG
                   MOVE '*'            TO  WRK-INCONSIS
                   MOVE WRK-49369      TO  OUT-CONTA-ATTR
                   GO                  TO  2510-99-FIM
               END-IF
           END-IF.

           IF  (INP-CNPJ       NOT EQUAL  SPACES AND LOW-VALUES) OR
               (INP-DIG        NOT EQUAL  SPACES AND LOW-VALUES)
                PERFORM  2515-CONSISTE-CPF
           END-IF.

      *----------------------------------------------------------------*
       2510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2513-ACESSAR-DCOM6408           SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6408-AREA
                      ROTSAI-6408-RETORNO.

           MOVE WRK-COD-USER-R         TO ROTENT-6408-CFUNC-ENV
           MOVE LNK-IO-LTERM           TO ROTENT-6408-CTERM-ENV
           IF  INP-CNPJ            NOT EQUAL SPACES AND  LOW-VALUES
               MOVE INP-CNPJ-N         TO ROTENT-6408-NUM-CNPJ-ENV
               IF  INP-FILIAL          EQUAL SPACES OR LOW-VALUES
                   MOVE ZEROS          TO ROTENT-6408-FIL-CNPJ-ENV
               ELSE
                   MOVE INP-FILIAL-N   TO ROTENT-6408-FIL-CNPJ-ENV
               END-IF
               MOVE INP-DIG-N          TO ROTENT-6408-CTR-CNPJ-ENV
           ELSE
               MOVE INP-AGENC-N        TO ROTENT-6408-CJUNC-ENV
               MOVE INP-CONTA-N        TO ROTENT-6408-CCTAC-ENV
                                          WRK-CONTA-0431
               MOVE 07                 TO WRK-TAMANHO-0431
               MOVE SPACES             TO WRK-DIGITO-0431

               CALL 'BRAD0431'         USING WRK-CONTA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431
               IF  WRK-DIGITO-0431     EQUAL '.'
                   MOVE  'ERRO NO ACESSO A BRAD0431'
                                       TO ERR-TEXTO
                   MOVE 'APL'          TO ERR-TIPO-ACESSO
                   PERFORM 9999-PROCESSAR-ROTINA-ERRO
               ELSE
                   IF  WRK-DIGITO-0431 EQUAL 'P'
                       MOVE '0'        TO WRK-DIGITO-0431
                   END-IF
               END-IF
               MOVE WRK-DIGITO-0431    TO ROTENT-6408-DCTAC-ENV
           END-IF.

           MOVE 'DCOM6408'             TO WRK-MODULO.
           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  ROTSAI-6408-COD-RET
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9999-PROCESSAR-ROTINA-ERRO
               WHEN  OTHER
                     MOVE '*'            TO WRK-INCONSIS
                 IF  ROTSAI-6408-COD-RET EQUAL '0003'
                     IF  INP-CNPJ NOT EQUAL SPACES AND LOW-VALUES
                        MOVE 'CPF/CNPJ INEXISTENTE'
                                         TO OUT-MENSAG
                        MOVE WRK-49369   TO OUT-DIG-ATTR
                                            OUT-FILIAL-ATTR
                                            OUT-CNPJ-ATTR
                     ELSE
                        MOVE 'AGENCIA/CONTA INEXISTENTE'
                                         TO OUT-MENSAG
                        MOVE WRK-49369   TO OUT-AGENC-ATTR
                                            OUT-CONTA-ATTR
                     END-IF
                 ELSE
                     MOVE ROTSAI-6408-MENS-RET
                                         TO OUT-MENSAG
                 END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       2513-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2515-CONSISTE-CPF               SECTION.
      *----------------------------------------------------------------*

FS2511*    MOVE INP-CNPJ-N           TO  WRK-NUMERO.

FS2511*    IF   INP-FILIAL           EQUAL SPACES  OR
FS2511*         INP-FILIAL-N         NOT NUMERIC
FS2511*         MOVE ZEROS           TO  WRK-FILIAL
FS2511*    ELSE
FS2511*         MOVE INP-FILIAL-N    TO  WRK-FILIAL
FS2511*    END-IF.

FS2511*    MOVE SPACES               TO  WRK-CONTROLE.

FS2511*    CALL 'BRAD0110' USING    WRK-CPFCGC
FS2511*                             WRK-CONTROLE.

FS2511*     IF  RETURN-CODE         EQUAL  12
FS2511*         MOVE  '*'            TO  WRK-INCONSIS
FS2511*         MOVE  WRK-49369      TO  OUT-CNPJ-ATTR
FS2511*                                  OUT-FILIAL-ATTR
FS2511*                                  OUT-DIG-ATTR
FS2511*         MOVE  SG-MENSAGEM (05)  TO OUT-MENSAG
FS2511*         GO                   TO  2515-99-FIM
FS2511*     END-IF.

FS2511*     IF  INP-DIG-N       NOT EQUAL WRK-CONTROLE
FS2511*         MOVE  '*'            TO   WRK-INCONSIS
FS2511*         MOVE  WRK-49369      TO   OUT-CNPJ-ATTR
FS2511*                                   OUT-FILIAL-ATTR
FS2511*                                   OUT-DIG-ATTR
FS2511*         MOVE  SG-MENSAGEM (15)    TO OUT-MENSAG
FS2511*         GO                   TO  2515-99-FIM
FS2511*     END-IF.

FS2511     MOVE INP-CNPJ-N           TO  WRK-CAD-NUMERO.

FS2511     IF   INP-FILIAL           EQUAL SPACES  OR
FS2511          INP-FILIAL-N         EQUAL SPACES OR LOW-VALUES
FS2511          MOVE SPACES          TO  WRK-CAD-FILIAL
FS2511     ELSE
FS2511          MOVE INP-FILIAL-N    TO  WRK-CAD-FILIAL
FS2511     END-IF.

FS2511     MOVE ZEROS                TO  WRK-CAD-CONTROLE.

FS2511     CALL WRK-BRAD2000      USING WRK-CAD-CPFCGC
FS2511                                   WRK-CAD-CONTROLE.

FS2511     IF  RETURN-CODE         NOT  EQUAL  ZEROS
FS2511         MOVE  '*'            TO  WRK-INCONSIS
FS2511         MOVE  WRK-49369      TO  OUT-CNPJ-ATTR
FS2511                                  OUT-FILIAL-ATTR
FS2511                                  OUT-DIG-ATTR
FS2511         MOVE  SG-MENSAGEM (05)  TO OUT-MENSAG
FS2511         GO                   TO  2515-99-FIM
FS2511     END-IF.

FS2511     IF  INP-DIG-N       NOT EQUAL WRK-CAD-CONTROLE
FS2511         MOVE  '*'            TO   WRK-INCONSIS
FS2511         MOVE  WRK-49369      TO   OUT-CNPJ-ATTR
FS2511                                   OUT-FILIAL-ATTR
FS2511                                   OUT-DIG-ATTR
FS2511         MOVE  SG-MENSAGEM (15)    TO OUT-MENSAG
FS2511         GO                   TO  2515-99-FIM
FS2511     END-IF.

           PERFORM 2513-ACESSAR-DCOM6408.

      *----------------------------------------------------------------*
       2515-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2520-TRATAR-LIBERAR             SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER             NOT EQUAL  SPACES
              PERFORM 2550-PASSAR-CONTROLE-OPER
              MOVE  'DCOM0416'         TO  WRK-TELA
              GO                       TO  2520-99-FIM
           END-IF.

           IF INP-AGENCIA          NOT EQUAL  SPACES
              PERFORM 2299-VALIDAR-PERFIL
              IF  WRK-INCONSIS         EQUAL SPACES
                  PERFORM 2560-PASSAR-CONTROLE-AGENCIA
                  MOVE  'DCOM0414'         TO  WRK-TELA
              END-IF
              GO                       TO  2520-99-FIM
           END-IF.

           IF (INP-AGENC           NOT EQUAL  SPACES) AND
              (INP-CONTA           NOT EQUAL  SPACES) OR
              (INP-CNPJ            NOT EQUAL  SPACES)
              MOVE SPACES          TO WRK-INCONSIS
              IF  INP-AGENC        NOT EQUAL SPACES
                  PERFORM 2299-VALIDAR-PERFIL
              END-IF
              IF  WRK-INCONSIS         EQUAL SPACES
                  PERFORM 2570-PASSAR-CONTROLE-CLIENTE
                  MOVE  'DCOM0415'         TO  WRK-TELA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2530-TRATAR-CANCELAR            SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER             NOT EQUAL  SPACES
              PERFORM 2550-PASSAR-CONTROLE-OPER
              MOVE  'DCOM0419'         TO  WRK-TELA
              GO                       TO  2530-99-FIM
           END-IF.

           IF INP-AGENCIA          NOT EQUAL  SPACES
              PERFORM 2299-VALIDAR-PERFIL
              IF  WRK-INCONSIS         EQUAL SPACES
                  PERFORM 2560-PASSAR-CONTROLE-AGENCIA
                  MOVE  'DCOM0417'     TO  WRK-TELA
              END-IF
              GO                       TO  2530-99-FIM
           END-IF.

           IF (INP-AGENC           NOT EQUAL  SPACES) AND
              (INP-CONTA           NOT EQUAL  SPACES) OR
              (INP-CNPJ            NOT EQUAL  SPACES)
              IF  INP-AGENC        NOT EQUAL SPACES
                  PERFORM 2299-VALIDAR-PERFIL
              END-IF
              IF  WRK-INCONSIS         EQUAL SPACES
                  PERFORM 2570-PASSAR-CONTROLE-CLIENTE
                  MOVE  'DCOM0418'         TO  WRK-TELA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2540-TRATAR-CONSULTA            SECTION.
      *----------------------------------------------------------------*

           IF INP-OPER             NOT EQUAL  SPACES
              PERFORM 2550-PASSAR-CONTROLE-OPER
              MOVE  'DCOM0422'         TO  WRK-TELA
              GO                       TO  2540-99-FIM
           END-IF.

           IF INP-AGENCIA          NOT EQUAL  SPACES
              PERFORM 2299-VALIDAR-PERFIL
              IF   WRK-INCONSIS        EQUAL SPACES
                   PERFORM 2560-PASSAR-CONTROLE-AGENCIA
                   MOVE  'DCOM0420'    TO  WRK-TELA
              END-IF
              GO                       TO  2540-99-FIM
           END-IF.

           IF (INP-AGENC           NOT EQUAL  SPACES) AND
              (INP-CONTA           NOT EQUAL  SPACES) OR
              (INP-CNPJ            NOT EQUAL  SPACES)

              IF  INP-AGENC        NOT EQUAL SPACES
                  PERFORM 2299-VALIDAR-PERFIL
              END-IF
              IF  WRK-INCONSIS         EQUAL SPACES
                  IF  INP-FILIAL       EQUAL SPACES OR LOW-VALUES
                      MOVE ZEROS       TO  INP-FILIAL
                  END-IF
                  PERFORM 2570-PASSAR-CONTROLE-CLIENTE
                  MOVE  'DCOM0421'         TO  WRK-TELA
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       2540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2550-PASSAR-CONTROLE-OPER       SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     COMU-AREA-416.

           MOVE  LENGTH COMU-AREA-416  TO COMU-LL-416.
           MOVE  INP-SENHAS            TO COMU-SENHAS-416.
           MOVE  INP-OPER-N            TO COMU-OPERACAO-416.
           MOVE  INP-OPCAO-N           TO COMU-OPCAO-416.
           MOVE  INP-TMSTAMP           TO COMU-TMSTAMP-416.
           MOVE  'DCOM0413'            TO COMU-TRANSACAO-416.
           MOVE  COMU-AREA-416         TO WRK-MENSAGEM.
           MOVE  WRK-CHNG              TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2560-PASSAR-CONTROLE-AGENCIA    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     COMU-AREA-414.

           MOVE  LENGTH COMU-AREA-414  TO COMU-LL-414
           MOVE  INP-SENHAS            TO COMU-SENHAS-414.
           MOVE  INP-COMANDO           TO COMU-COMANDO-414.
           MOVE  INP-AGENCIA-N         TO COMU-CODAGE-414.
           MOVE  INP-OPCAO-N           TO COMU-OPCAO-414.
           MOVE  INP-TMSTAMP           TO COMU-TMSTAMP-414.
           MOVE  'DCOM0413'            TO COMU-TRANSACAO-414.
           MOVE  COMU-AREA-414         TO WRK-MENSAGEM.
           MOVE  WRK-CHNG              TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2570-PASSAR-CONTROLE-CLIENTE    SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                     COMU-AREA-415.

           MOVE  LENGTH COMU-AREA-415  TO COMU-LL-415.
           MOVE  INP-SENHAS            TO COMU-SENHAS-415.
           MOVE  INP-COMANDO           TO COMU-COMANDO-415.
           MOVE  INP-AGENC-N           TO COMU-CODAGE-415.
           MOVE  INP-CONTA-N           TO COMU-CONTA-415.
           MOVE  INP-CNPJ-N            TO COMU-NUMERO-415.
           MOVE  INP-FILIAL-N          TO COMU-FILIAL-415.
           MOVE  INP-DIG-N             TO COMU-CONTROLE-415.
           MOVE  INP-OPCAO-N           TO COMU-OPCAO-415.
           MOVE  INP-TMSTAMP           TO COMU-TMSTAMP-415.
           MOVE  'DCOM0413'            TO COMU-TRANSACAO-415.
           MOVE  COMU-AREA-415         TO WRK-MENSAGEM.
           MOVE  WRK-CHNG              TO WRK-FUNCAO.

      *----------------------------------------------------------------*
       2570-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       2580-VER-DISPONIBILIDADE        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE ROTENT-6438-ENVIO.

           MOVE 'N'                    TO ROTENT-6438-FLAG.
           MOVE WRK-COD-USER           TO ROTENT-6438-CFUNC-ENV.
           MOVE LNK-IO-LTERM           TO ROTENT-6438-CTERM.
           MOVE 2                      TO ROTENT-6438-AGEUSU-ENV.
           MOVE ZEROS                  TO ROTENT-6438-PROD-ENV.
           MOVE 'M'                    TO ROTENT-6438-PROC-ENV.

           EVALUATE  INP-OPCAO
               WHEN '1'
                     MOVE 'I'          TO ROTENT-6438-FUNC-ENV

               WHEN '2'
                     MOVE 'E'          TO ROTENT-6438-FUNC-ENV

               WHEN '3'
                     MOVE 'C'          TO ROTENT-6438-FUNC-ENV
           END-EVALUATE.

      *--> LIBERACAO DO CONTRATO LIMITE
           MOVE 009                    TO ROTENT-6438-OPCAO-ENV
           MOVE 'OP'                   TO ROTENT-6438-MOT-ENV.
           MOVE 'DCOM6438'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6438-ENVIO
                                             ROTSAI-6438-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           IF  ROTSAI-6438-COD-RET     EQUAL '0099'
               MOVE 'DB2'              TO ERR-TIPO-ACESSO
               PERFORM 9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           IF  ROTSAI-6438-COD-RET     EQUAL ZEROS AND
               ROTSAI-6438-RESUL-RET   EQUAL 1
               NEXT SENTENCE
           ELSE
               MOVE '*'                TO WRK-INCONSIS
               MOVE ROTSAI-6438-MENS-RET
                                       TO OUT-MENSAG
           END-IF.

      *----------------------------------------------------------------*
       2580-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       3000-PROCESSAR-BRAD0660         SECTION.
      *----------------------------------------------------------------*

           CALL  'BRAD0660'            USING  OUTPUT-DMTM
                                              660-DCOMDMTM.

           IF RETURN-CODE              NOT EQUAL  ZEROS
              MOVE  'APL'              TO  ERR-TIPO-ACESSO
              MOVE  RETURN-CODE        TO  WRK-RETURN-CODE
              MOVE  10                 TO  WRK-LOCAL-ERRO
              MOVE  WRK-ERRO-BRAD0660  TO  ERR-TEXTO
              PERFORM  9999-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE  WRK-ISRT              TO  WRK-FUNCAO.
           MOVE  OUTPUT-DMTM           TO  WRK-MENSAGEM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       9999-PROCESSAR-ROTINA-ERRO      SECTION.
      *----------------------------------------------------------------*

           IF ERR-MODULO      NOT EQUAL  SPACES  AND  LOW-VALUES
              MOVE  'DCOM0413'         TO  ERR-PGM
           ELSE
              MOVE  'SENHAS02'         TO  ERR-PGM
              MOVE  'DCOM0413'         TO  ERR-MODULO
           END-IF.

           MOVE  WRK-COD-USER          TO  ERR-COD-USER.
           MOVE  WRK-COD-DEPTO         TO  ERR-COD-DEPTO.


           IF  ERR-TIPO-ACESSO         EQUAL 'DB2'
               CALL  'BRAD7100'        USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
                                              WRK-SQLCA
           ELSE
               CALL 'BRAD7100'         USING  LNK-IO-PCB
                                              ERRO-AREA
                                              LNK-ALT-PCB
           END-IF.

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
