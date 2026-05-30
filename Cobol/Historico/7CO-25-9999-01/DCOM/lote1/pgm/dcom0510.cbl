      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM0510.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                        A L T R A N                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0510                                    *
      *    PROGRAMADOR.:   FSW - ALCIONE                               *
      *    ANALISTA ...:   EMERSON ARTUR FRANCO                        *
      *    ANALISTA.BCO:   EDGARD RODRIGUES                            *
      *    DATA........:   22/09/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ESTORNO DE CONTRATO LIMITE POR CLIENTE      *
      *                                                                *
      *----------------------------------------------------------------*
      *    TELA........:   DCOMEJTM - ESTORNO DE CONTRATO POR CLIENTE  *
      *----------------------------------------------------------------*
      *    BOOK'S......:                                               *
      *    I#BRAD7C - TRATAMENTO DE ERROS DA APLICACAO                 *
      *    I#DCOMSY - TRATAMENTO MENSAGENS DO SISTEMS DCOM(ON-LINE)    *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    POOL5005 - CONTROLE DO TRANSITO DE MENSAGENS DO IMS/DC.     *
      *    BRAD0660 - COMPACTAR MENSAGENS DE NAVEGACAO E TRANSMISSAO.  *
      *    BRAD7100 - TRATAMENTO DE ERROS DA APLICACAO.                *
      *    DCOM4310 - MODULO DE NEGOCIO - EXCLUSAO - CONSULTA.         *
      *    DCOM4370 - MODULO DE NEGOCIO - INCLUSAO                     *
      *    DCOM6416 - MODULO DE NEGOCIO - BUSCA NOME AGENCIA           *
      *----------------------------------------------------------------*
      *    NAVEGACAO...:                                               *
      *            CHAMADO POR                         CHAMA           *
      *               DCOM0508                       DCOM0511          *
      *                  -                           DCOM0512          *
      *                  -                           DCOM0513          *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *================================================================*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
      *================================================================*
       WORKING-STORAGE                 SECTION.
      *================================================================*
       01  FILLER                      PIC  X(032)  VALUE
           '*** INICIO DA WORKING STORAGE **'.
      *----------------------------------------------------------------*
       01  WRK-COMU-LENG               PIC S9(004)  COMP  VALUE +56.
       01  WRK-INCONSIS                PIC  X(001)  VALUE SPACES.
       01  WRK-COUNT                   PIC  9(001)  VALUE ZEROS.
       01  WRK-IND                     PIC  9(002)  VALUE ZEROS.
       01  WRK-INDT                    PIC  9(002)  VALUE ZEROS.
       01  WRK-RESTART                 PIC  9(005)  VALUE ZEROS.
       01  WRK-MODULO                  PIC  X(008)  VALUE SPACES.
       01  WRK-SQLCA                   PIC  X(136)  VALUE SPACES.
       01  WRK-ENCONTROU               PIC  9(001)  VALUE ZEROS.
           88  NOT-FOUND                            VALUE 1.

       01  WRK-OPERACAO                PIC  9(013)  VALUE ZEROS.
       01  FILLER    REDEFINES    WRK-OPERACAO.
           03  WRK-ANO-OPER            PIC  9(004).
           03  WRK-SEQ-OPER            PIC  9(009).

       01  WRK-MSG01                   PIC  X(079)  VALUE
            'NAO HA OCORRENCIAS PARA SEREM MOSTRADAS'.
      *
      *----------------------------------------------------------------*
      *--- ATRIBUTOS PARA VARIAVEIS DE TELA                        ----*
      *--- ALF - NOR  -DESP - DISP                       (+193)    ----*
      *--- ALF - BRI - DESP - DISP                       (+201)    ----*
      *--- ALF - NOR - PROT - DISP                       (+225)    ----*
      *--- ALF - BRI - DESP - DISP - CURSOR              (+49353)  ----*
      *----------------------------------------------------------------*
       01  WRK-193                     PIC S9(008)  COMP VALUE +193.
       01  WRK-201                     PIC S9(008)  COMP VALUE +201.
       01  WRK-225                     PIC S9(008)  COMP VALUE +225.
       01  WRK-49353                   PIC S9(008)  COMP VALUE +49353.
       01  WRK-49345                   PIC S9(008)  COMP VALUE +49345.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '** VARIAVEIS POOL5005 **'.
      *----------------------------------------------------------------*
       01  WRK-POOL5005.
           05  WRK-GU                  PIC  X(04)   VALUE 'GU  '.
           05  WRK-CHNG                PIC  X(04)   VALUE 'CHNG'.
           05  WRK-ISRT                PIC  X(04)   VALUE 'ISRT'.
           05  WRK-FUNCAO              PIC  X(04)   VALUE  SPACES.
           05  WRK-TELA                PIC  X(08)   VALUE  SPACES.
           05  WRK-VERSAO              PIC  X(06)   VALUE 'VRS002'.
           05  WRK-COD-USER            PIC  X(07)   VALUE  SPACES.
           05  WRK-COD-DEPTO.
               10  FILLER                  PIC  X(02)   VALUE  SPACES.
               10  WRK-COD-DEPTO-N         PIC  9(04)   VALUE  ZEROS.

           05  WRK-MENSAGEM.
               10  FILLER              PIC  X(04)   VALUE  SPACES.
               10  WRK-TRANSACAO       PIC  X(08)   VALUE  SPACES.
               10  FILLER              PIC  X(1988) VALUE  SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA MENSAGENS DCOM **'.
      *----------------------------------------------------------------*
      *
           COPY 'I#DCOMSY'.
      *
      *================================================================*
       01  FILLER                      PIC  X(050)  VALUE
           '* AREA VARIAVEIS AXILIARES *'.
      *----------------------------------------------------------------*

       01  WRK-TIMESTAMP               PIC  X(026)  VALUE SPACES.
       01  WRK-LINHA-AGENCIA.
           05  FILLER                  PIC  X(021)  VALUE
               'BANCO: 237  AGENCIA: '.
           05  WRK-AGENCIA-TELA.
               10  WRK-AGENCIA-NUM     PIC  9(004)  VALUE ZEROS.
               10  FILLER              PIC  X(001)  VALUE '-'.
               10  WRK-NOME-AGENCIA    PIC  X(021)  VALUE SPACES.
           05  FILLER                  PIC  X(002)  VALUE SPACES.
           05  FILLER                  PIC  X(011)  VALUE
               'CONTA....: '.
           05  WRK-CONTA               PIC  9(013)  VALUE ZEROS.

       01  WRK-LINHA-CPF-CNPJ.
           05  FILLER                  PIC  X(007)  VALUE
               'NOME.: '.
           05  WRK-NOME                PIC  X(030)  VALUE SPACES.
           05  FILLER                  PIC  X(010)  VALUE SPACES.
           05  WRK-LT-CNPJ             PIC  X(010)  VALUE 'CNPJ/CPF: '.
           05  WRK-CPF-CNPJ.
               10  WRK-NUMERO-X.
                   15  WRK-NUMERO      PIC  999.999.999 VALUE ZEROS.
               10  FILLER              PIC  X(001)  VALUE '/'.
FS2511*        10  WRK-FILIAL          PIC  9(004)  VALUE ZEROS.
FS2511         10  WRK-FILIAL          PIC  X(04)   VALUE SPACES.
               10  FILLER              PIC  X(001)  VALUE '-'.
               10  WRK-CONTROLE        PIC  9(002)  VALUE ZEROS.

       01  WRK-AREA-BRAD0431.
           05 WRK-CONTA-0431           PIC  9(013) COMP-3  VALUE ZEROS.
           05 WRK-TAMANHO-0431         PIC  9(002)         VALUE ZEROS.
           05 WRK-DIGITO-0431          PIC  X(001)         VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ERROS APLICACAO *'.
      *----------------------------------------------------------------*
      *
           COPY 'I#BRAD7C'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)         VALUE
           '*** AREA DE COMUNICACAO DCOM6408 ***'.
      *----------------------------------------------------------------*

       COPY 'I#DCOMX3'.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ROTINA DCOM475X *'.
      *----------------------------------------------------------------*
      *
       01  475XE-ROTEADOR.
           03  475X-E-HEADER.
               05  475X-E-COD-RETORNO             PIC  X(004).
               05  475X-E-MSG-RETORNO             PIC  X(079).
               05  475X-E-RESTART                 PIC  9(005).
               05  475X-E-FLAG                    PIC  X(001).
               05  475X-E-START-COUNT             PIC  X(001).
               05  475X-E-CFUNC-BDSCO             PIC  9(009).
               05  475X-E-CTERM                   PIC  X(008).
           03  475X-E-DADOS.
               10  475X-E-FLAG-FILTRO             PIC  X(002).
               10  475X-E-CBCO                    PIC  9(003).
               10  475X-E-CAG-BCRIA               PIC  9(005).
               10  475X-E-CCTA-BCRIA-CLI          PIC  9(013).
               10  475X-E-DINIC-OPER-DESC         PIC  X(010).
FS2511*        10  475X-E-CCNPJ-CPF               PIC  9(009).
FS2511         10  475X-E-CCNPJ-CPF               PIC  X(009).
FS2511*        10  475X-E-CFLIAL-CNPJ             PIC  9(005).
FS2511         10  475X-E-CFLIAL-CNPJ             PIC  X(004).
               10  475X-E-CCTRL-CNPJ-CPF          PIC  9(002).
               10  475X-E-CSIT-DESC-COML          PIC  9(003).

       01  475XS-ROTEADOR.
           03  475X-S-HEADER.
               05  475X-S-COD-RETORNO      PIC  X(004).
               05  475X-S-MSG-RETORNO      PIC  X(079).
               05  475X-S-RESTART          PIC  9(005).
               05  475X-S-FLAG             PIC  X(001).
               05  475X-S-QTDE-RETORNADA   PIC  9(003).
               05  475X-S-COUNT            PIC  9(009).

           03  475X-S-DADOS.
               05  475X-S-TABELA-SAIDA     OCCURS 10  TIMES.
                   10  475X-S-CCONTR                PIC  9(009).
                   10  475X-S-CVRSAO-CONTR-LIM      PIC  9(003).
                   10  475X-S-IPSSOA-DESC-COML      PIC  X(060).
                   10  475X-S-DINIC-VGCIA-CONTR     PIC  X(010).
                   10  475X-S-DVCTO-CONTR-LIM       PIC  X(010).
                   10  475X-S-VCONTR-LIM-DESC       PIC  9(015)V99.
                   10  475X-S-CSIT-DESC-COML        PIC  9(003).
                   10  475X-S-ISIT-DESC-COML        PIC  X(030).
                   10  475X-S-IRSUMO-SIT-DESC       PIC  X(015).
                   10  475X-S-CSGL-SIT-DESC         PIC  X(006).

      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ROTINA DCOM6416 *'.
      *----------------------------------------------------------------*

       01  ENTRADA-ROTEADOR-6416.
           03  ROTENT-COD-RETORNO-6416                 PIC X(04).
           03  ROTENT-MSG-RETORNO-6416                 PIC X(79).
           03  ROTENT-RESTART-6416                     PIC 9(05).
           03  ROTENT-FLAG-6416                        PIC X(01).
           03  ROTENT-CFUNC-BDSCO-6416                 PIC 9(09).
           03  ROTENT-CTERM-6416                       PIC X(08).
           03  ROTENT-BANCO-6416                       PIC 9(03).
           03  ROTENT-DEPDC-6416                       PIC 9(05).

       01  SAIDA-ROTEADOR-6416.
           03  ROTSAI-HEADER-6416.
               05  ROTSAI-COD-RETORNO-6416             PIC X(04).
               05  ROTSAI-MSG-RETORNO-6416             PIC X(79).
               05  ROTSAI-RESTART-6416                 PIC 9(05).
               05  ROTSAI-FLAG-6416                    PIC X(01).
           03  ROTSAI-DADOS-6416.
               05  ROTSAI-TABELA-SAIDA-6416.
                   07  ROTSAI-AG-DEPDC-6416            PIC 9(05).
                   07  ROTSAI-DG-DEPDC-6416            PIC X(01).
                   07  ROTSAI-NOME-DEPDC-6416          PIC X(40).
                   07  ROTSAI-END-DEPDC-6416           PIC X(30).
                   07  ROTSAI-DIRETORIA-REG-6416       PIC 9(05).
                   07  ROTSAI-DIRETORIA-NOME-6416      PIC X(40).
                   07  ROTSAI-NOME-BANCO-6416          PIC X(40).
                   07  ROTSAI-POLO-SERVICO-6416        PIC 9(05).
                   07  ROTSAI-MUNIC-DEPDC-6416         PIC X(25).
                   07  ROTSAI-UF-6416                  PIC X(02).
                   07  ROTSAI-CEP-6416                 PIC 9(05).
                   07  ROTSAI-CEP-COMPL-6416           PIC 9(03).
                   07  ROTSAI-INDIC-DEPDC-AT-6416      PIC X(01).
                   07  ROTSAI-NATUREZA-DEPDC-6416      PIC X(01).
                   07  ROTSAI-COD-DISTR-CORR-6416      PIC 9(05).
                   07  ROTSAI-DENCRR-DEPDC-BDSCO-6416  PIC X(10).
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ERROS BRAD0660 **'.
      *----------------------------------------------------------------*
       01  WRK-ERRO-BRAD0660.
           05 FILLER                   PIC  X(045)  VALUE
              'RETURN-CODE DIFERENTE DE ZEROS NA BRAD0660'.
           05 FILLER                   PIC  X(012)  VALUE
              '- RET.COD ='.
           05 WRK-RETURN-CODE          PIC  9(002)  VALUE ZEROS.
           05 FILLER                   PIC  X(011)  VALUE
              ' - LOCAL ='.
           05 WRK-LOCAL-ERRO           PIC  9(002)  VALUE ZEROS.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '*** AREA COMUNICACAO ***'.
      *----------------------------------------------------------------*
       01  COMU-AREA.
           05  COMU-LL                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-ZZ                 PIC S9(004) COMP    VALUE ZEROS.
           05  COMU-TRANCODE.
               10  COMU-TRANSACAO      PIC  X(008)         VALUE SPACES.
               10  FILLER              PIC  X(006)         VALUE SPACES.
               10  COMU-PFK            PIC  X(001)         VALUE SPACES.
           05  COMU-SENHAS             PIC  X(037)         VALUE SPACES.
           05  COMU-COMANDO            PIC  X(068)         VALUE SPACES.
           05  COMU-DADOS.
               10  COMU-TPOPCAO        PIC  X(001)         VALUE SPACES.
               10  COMU-TIMESTAMP      PIC  X(026)         VALUE SPACES.
FS2511*        10  COMU-CNPJCPF        PIC  9(009)         VALUE ZEROS.
FS2511         10  COMU-CNPJCPF        PIC  X(09)          VALUE SPACES.
FS2511*        10  COMU-FILIAL         PIC  9(004)         VALUE ZEROS.
FS2511         10  COMU-FILIAL         PIC  X(04)          VALUE SPACES.
               10  COMU-CONTROLE       PIC  9(002)         VALUE ZEROS.
               10  COMU-BANCO          PIC  9(003)         VALUE ZEROS.
               10  COMU-AGENCIA        PIC  9(005)         VALUE ZEROS.
               10  COMU-CONTA          PIC  9(013)         VALUE ZEROS.
               10  COMU-OPERACAO       PIC  9(013)         VALUE ZEROS.
               10  FILLER REDEFINES    COMU-OPERACAO.
                   15 COMU-DANO        PIC  9(004).
                   15 COMU-NSEQ        PIC  9(009).
               10  COMU-CONTRATO-LIM   PIC  9(009)         VALUE ZEROS.
               10  COMU-VERSAO         PIC  9(003)         VALUE ZEROS.
               10  COMU-PAGINA1        PIC  9(004)         VALUE ZEROS.
               10  COMU-OCCUR          PIC  9(008)         VALUE ZEROS.
           05  COMU-MENSAGEM           PIC  X(079)         VALUE SPACES.
           05  COMU-ACAO               PIC  X(001)         VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA INPUT DCOMEJTM **'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-EJTM.
           03  FILLER                  PIC X(018)  VALUE SPACES.
           03  WRK-INP-PFK-EJTM        PIC X(001)  VALUE SPACES.
           03  WRK-INP-DADOS-EJTM.
               05  WRK-INP-SENHAS-EJTM PIC  X(037) VALUE SPACES.
               05  WRK-INP-COMANDO-EJTM   PIC  X(068) VALUE SPACES.
               05  WRK-INP-ESCOND-EJTM.
                   07  WRK-INP-OPCAO-EJTM PIC  9(001) VALUE ZEROS.
                   07  WRK-INP-CPAG-EJTM  PIC  9(003) VALUE ZEROS.
                   07  WRK-INP-QTIP-EJTM  PIC  9(001) VALUE ZEROS.
                   07  WRK-INP-FIMP-EJTM  PIC  X(001) VALUE SPACES.
                   07  WRK-INP-TPLI-EJTM  PIC  X(001) VALUE SPACES.
                   07  WRK-INP-VADO-EJTM  PIC  X(023) VALUE SPACES.
               05  WRK-INP-TIMEST-EJTM    PIC  X(026) VALUE SPACES.
               05  WRK-INP-PAG-EJTM.
                   07  WRK-INP-PAG-EJTM-N PIC  9(003) VALUE ZEROS.
               05  WRK-INP-LINHA1-EJTM    PIC  X(079) VALUE SPACES.
               05  WRK-INP-LINHA2-EJTM    PIC  X(079) VALUE SPACES.
               05  WRK-INP-TAB-EJTM   OCCURS   10  TIMES.
                   07  WRK-INP-SEL-EJTM   PIC  X(001).
                   07  WRK-INP-NUME-EJTM  PIC  X(009).
                   07  WRK-INP-VERS-EJTM  PIC  X(003).
                   07  WRK-INP-NOME-EJTM  PIC  X(018).
                   07  WRK-INP-DTIN-EJTM  PIC  X(010).
                   07  WRK-INP-DTVC-EJTM  PIC  X(010).
                   07  WRK-INP-VLIM-EJTM  PIC  X(014).
                   07  WRK-INP-SITU-EJTM  PIC  X(006).
               05  WRK-INP-MSGEM-EJTM     PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA OUTPUT DCOMEJTM *'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-EJTM.
           03  WRK-OUT-LL-EJTM         PIC S9(004)  COMP VALUE +0000.
           03  WRK-OUT-ZZ-EJTM         PIC S9(004)  COMP VALUE ZEROS.
           03  WRK-OUT-DADOS-EJTM.
               05  WRK-OUT-SENHAS-EJTM  PIC  X(037) VALUE SPACES.
               05  WRK-OUT-COMANDO-EJTM PIC  X(068) VALUE SPACES.
               05  WRK-OUT-ESCOND-EJTM.
                   07  WRK-OUT-OPCAO-EJTM PIC  9(001) VALUE ZEROS.
                   07  WRK-OUT-CPAG-EJTM  PIC  9(003) VALUE ZEROS.
                   07  WRK-OUT-QTIP-EJTM  PIC  9(001) VALUE ZEROS.
                   07  WRK-OUT-FIMP-EJTM  PIC  X(001) VALUE SPACES.
                   07  WRK-OUT-TPLI-EJTM  PIC  X(001) VALUE SPACES.
                   07  WRK-OUT-VAGO-EJTM  PIC  X(023) VALUE SPACES.
               05  WRK-OUT-TIMEST-EJTM     PIC  X(026) VALUE SPACES.
               05  WRK-OUT-PAG-EJTM.
                   07  WRK-OUT-PAG-EJTM-N PIC 9(03) VALUE ZEROS.
               05  WRK-OUT-LINHA1-EJTM    PIC  X(079) VALUE SPACES.
               05  WRK-OUT-LINHA2-EJTM    PIC  X(079) VALUE SPACES.
               05  WRK-OUT-TAB-EJTM   OCCURS   10  TIMES.
                   07  WRK-OUT-SEL-ATTR   PIC S9(004) COMP.
                   07  WRK-OUT-SEL-EJTM   PIC  X(001).
                   07  WRK-OUT-NUME-EJTM  PIC  X(009).
                   07  WRK-OUT-VERS-EJTM  PIC  X(003).
                   07  WRK-OUT-NOME-EJTM  PIC  X(018).
                   07  WRK-OUT-DTIN-EJTM  PIC  X(010).
                   07  WRK-OUT-DTVC-EJTM  PIC  X(010).
                   07  WRK-OUT-VLIM-EJTM  PIC  X(014).
                   07  WRK-OUT-VLIM-EDIT  REDEFINES  WRK-OUT-VLIM-EJTM
                                          PIC  ZZZ.ZZZ.ZZ9,99.
                   07  WRK-OUT-SITU-EJTM  PIC  X(006).
               05  WRK-OUT-MSGEM-EJTM     PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA BRAD0660 (EJTM) *'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMEJTM.
           05  WRK-660-LL-AREA-EJTM        PIC  9(004) COMP VALUE  0000.
           05  WRK-660-LL-MENSAGEM-EJTM    PIC  9(004) COMP VALUE  0000.
           05  WRK-660-SENHAS-EJTM         PIC  9(004) COMP VALUE  0037.
           05  WRK-660-COMANDO-EJTM        PIC  9(004) COMP VALUE  1068.
           05  WRK-660-ESCOND-EJTM         PIC  9(004) COMP VALUE  0030.
           05  WRK-660-TIM-EJTM            PIC  9(004) COMP VALUE  0026.
           05  WRK-660-PAG-EJTM            PIC  9(004) COMP VALUE  0003.
           05  WRK-660-LINHA1-EJTM         PIC  9(004) COMP VALUE  0079.
           05  WRK-660-LINHA2-EJTM         PIC  9(004) COMP VALUE  0079.
           05  WRK-660-LINHA               OCCURS 10.
               10  WRK-660-SEL-EJTM        PIC  9(004) COMP VALUE  2003.
               10  WRK-660-NUME-EJTM       PIC  9(004) COMP VALUE  0009.
               10  WRK-660-VERS-EJTM       PIC  9(004) COMP VALUE  0003.
               10  WRK-660-NOME-EJTM       PIC  9(004) COMP VALUE  0018.
               10  WRK-660-DTIN-EJTM       PIC  9(004) COMP VALUE  0010.
               10  WRK-660-DTVC-EJTM       PIC  9(004) COMP VALUE  0010.
               10  WRK-660-VLIM-EJTM       PIC  9(004) COMP VALUE  0014.
               10  WRK-660-SITU-EJTM       PIC  9(004) COMP VALUE  0006.
           03  WRK-660-MSGEM-EJTM          PIC  9(004) COMP VALUE  0079.
      *
      *================================================================*
       01  FILLER                      PIC  X(032)  VALUE
           '*** FINAL DA WORKING STORAGE ***'.
      *----------------------------------------------------------------*
      *================================================================*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       01  LNK-IO-PCB.
           05 LNK-IO-LTERM             PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-IO-STATUS            PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-IO-MODNAME           PIC  X(008).

       01  LNK-ALT-PCB.
           05 LNK-ALT-LTERM            PIC  X(008).
           05 FILLER                   PIC  X(002).
           05 LNK-ALT-STATUS           PIC  X(002).
           05 FILLER                   PIC  X(012).
           05 LNK-ALT-MODNAME          PIC  X(008).
      *
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

           ENTRY  'DLITCBL'            USING       LNK-IO-PCB
                                                   LNK-ALT-PCB.

           MOVE    WRK-GU              TO          WRK-FUNCAO.

      ******************************************************************
      *    SECTION TRATAMENTO INICIAL DO PROGRAMA                      *
      ******************************************************************
       0000-00-INICIAR                 SECTION.
      *----------------------------------------------------------------*

           CALL   'POOL5005'           USING       WRK-VERSAO
                                                   WRK-FUNCAO
                                                   LNK-IO-PCB
                                                   LNK-ALT-PCB
                                                   WRK-MENSAGEM
                                                   WRK-TELA
                                                   WRK-COD-USER
                                                   WRK-COD-DEPTO.

           IF      RETURN-CODE         EQUAL       04  OR  12
                   PERFORM             9999-00-FINALIZAR
           END-IF.


           PERFORM 0100-00-PROCESSAR-MENSAGEM.

           GO      0000-00-INICIAR.

      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO IDENTIFICACAO DA MENSAGEM RECEBIDA       *
      ******************************************************************
       0100-00-PROCESSAR-MENSAGEM         SECTION.
      *----------------------------------------------------------------*

           MOVE    SPACES              TO          WRK-OUT-DADOS-EJTM
                                                   WRK-INCONSIS.

           INITIALIZE                              475XE-ROTEADOR
                                                   475XS-ROTEADOR.

           MOVE    'DCOMEJTM'          TO          WRK-TELA.

           INITIALIZE          COMU-AREA.

           IF      WRK-TRANSACAO       NOT EQUAL  'DCOM0510'
                   PERFORM             0200-00-RECEBER-CONTROLE
           ELSE
                   PERFORM             0500-00-PROCESSAR-DCOM0510
           END-IF.

           IF      WRK-TELA            EQUAL      'DCOMEJTM'
                   PERFORM             9500-00-PROCESSAR-BRAD0660
           END-IF.

      *----------------------------------------------------------------*
       0100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RECEBIMENTO DE MENSAGEM OUTROS           *
      ******************************************************************
       0200-00-RECEBER-CONTROLE        SECTION.
      *----------------------------------------------------------------*

           MOVE    SPACES              TO        WRK-INCONSIS.
           MOVE    WRK-MENSAGEM        TO        COMU-AREA.
           MOVE    COMU-TPOPCAO        TO        WRK-INP-OPCAO-EJTM.
           MOVE    COMU-TIMESTAMP      TO        WRK-OUT-TIMEST-EJTM
                                                 WRK-INP-TIMEST-EJTM.

           PERFORM 0201-00-ACESSAR-DCOM6408.

           IF  COMU-CNPJCPF            NOT EQUAL SPACES AND
               COMU-CNPJCPF            NOT EQUAL LOW-VALUES
               MOVE '1'                TO WRK-INP-TPLI-EJTM
                                          WRK-OUT-TPLI-EJTM
               MOVE COMU-CNPJCPF       TO WRK-NUMERO
               MOVE COMU-FILIAL        TO WRK-FILIAL
               MOVE COMU-CONTROLE      TO WRK-CONTROLE
               MOVE 'CNPJ/CPF: '       TO WRK-LT-CNPJ
               MOVE WRK-LINHA-CPF-CNPJ TO WRK-OUT-LINHA1-EJTM
                                          WRK-INP-LINHA1-EJTM
               MOVE SPACES             TO WRK-LINHA-AGENCIA
               MOVE WRK-LINHA-AGENCIA  TO WRK-OUT-LINHA2-EJTM
                                          WRK-INP-LINHA2-EJTM
           ELSE
               MOVE '2'                TO WRK-INP-TPLI-EJTM
                                          WRK-OUT-TPLI-EJTM
               MOVE SPACES             TO WRK-CPF-CNPJ
                                          WRK-LT-CNPJ
               MOVE WRK-LINHA-CPF-CNPJ TO WRK-OUT-LINHA1-EJTM
                                          WRK-INP-LINHA1-EJTM
               MOVE COMU-AGENCIA       TO WRK-AGENCIA-NUM
               PERFORM 0230-00-CH-DCOM6416
               MOVE COMU-CONTA         TO WRK-CONTA
               MOVE WRK-LINHA-AGENCIA  TO WRK-OUT-LINHA2-EJTM
                                          WRK-INP-LINHA2-EJTM
           END-IF.

           IF  WRK-TRANSACAO           EQUAL   'DCOM0508'
               MOVE    1               TO          WRK-INP-PAG-EJTM-N
           ELSE
               IF  COMU-PAGINA1        GREATER 1
                   COMPUTE 475X-E-RESTART  = (COMU-PAGINA1 - 1) * 10
                   MOVE    COMU-PAGINA1    TO      WRK-INP-PAG-EJTM-N
               ELSE
                   MOVE    1               TO      WRK-INP-PAG-EJTM-N
               END-IF
           END-IF.

           PERFORM 0210-00-CH-DCOM475X.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0200-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           MOVE    WRK-INP-ESCOND-EJTM    TO  WRK-OUT-ESCOND-EJTM.
           MOVE    WRK-INP-PAG-EJTM-N     TO  WRK-OUT-PAG-EJTM-N.

           IF  WRK-TRANSACAO               EQUAL     'DCOM0511' OR
                                                     'DCOM0512' OR
                                                     'DCOM0513'
               IF  COMU-MENSAGEM           NOT EQUAL  SPACES
                   MOVE  COMU-MENSAGEM         TO  WRK-OUT-MSGEM-EJTM
               ELSE
                   MOVE  SG-MENSAGEM(217)      TO  WRK-OUT-MSGEM-EJTM
               END-IF
           ELSE
               MOVE  SG-MENSAGEM(217)      TO  WRK-OUT-MSGEM-EJTM
           END-IF.

      *----------------------------------------------------------------*
       0200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA PARA OBTER NOME DO CLIENTE                  *
      ******************************************************************
       0201-00-ACESSAR-DCOM6408        SECTION.
      *----------------------------------------------------------------*

           PERFORM 0201-01-OBTER-DIGITO.

           INITIALIZE ROTENT-6408-AREA
                      ROTSAI-6408-RETORNO
                      ERRO-AREA.

FS2511     IF  COMU-CNPJCPF            NOT EQUAL SPACES AND
FS2511         COMU-CNPJCPF            NOT EQUAL LOW-VALUES
               MOVE COMU-CNPJCPF       TO ROTENT-6408-NUM-CNPJ-ENV
               MOVE COMU-FILIAL        TO ROTENT-6408-FIL-CNPJ-ENV
               MOVE COMU-CONTROLE      TO ROTENT-6408-CTR-CNPJ-ENV
           ELSE
               MOVE COMU-CONTA         TO ROTENT-6408-CCTAC-ENV
               MOVE COMU-AGENCIA       TO ROTENT-6408-CJUNC-ENV
               MOVE WRK-DIGITO-0431    TO ROTENT-6408-DCTAC-ENV
           END-IF.

           MOVE 'DCOM6408'             TO WRK-MODULO.

           CALL WRK-MODULO             USING ROTENT-6408-AREA
                                             ROTSAI-6408-RETORNO
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-6408-COD-RET
               WHEN '0000'
                    MOVE ROTSAI-6408-NM-RZSOC-RET(1)
                                       TO WRK-NOME

               WHEN '0099'
                    MOVE 'DB2'         TO ERR-TIPO-ACESSO
                    PERFORM 9995-00-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE ROTSAI-6408-MENS-RET
                                        TO          COMU-MENSAGEM
                   PERFORM             0520-00-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       0201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA PARA OBTER DIGITO DA CONTA                  *
      ******************************************************************
       0201-01-OBTER-DIGITO            SECTION.
      *----------------------------------------------------------------*

           MOVE COMU-CONTA             TO WRK-CONTA-0431
           MOVE 07                     TO WRK-TAMANHO-0431.
           MOVE SPACES                 TO WRK-DIGITO-0431.

           CALL 'BRAD0431'             USING WRK-CONTA-0431
                                             WRK-DIGITO-0431
                                             WRK-TAMANHO-0431.

           IF  WRK-DIGITO-0431         EQUAL '.'
               MOVE 'ERRO NO ACESSO A BRAD0431'
                                       TO ERR-TEXTO
               MOVE 'APL'              TO ERR-TIPO-ACESSO
               PERFORM 9995-00-PROCESSAR-ROTINA-ERRO
           ELSE
               IF  WRK-DIGITO-0431     EQUAL 'P'
                   MOVE '0'            TO WRK-DIGITO-0431
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       0201-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA DCOM475X                *
      ******************************************************************
       0210-00-CH-DCOM475X             SECTION.
      *----------------------------------------------------------------*

           EVALUATE  WRK-INP-OPCAO-EJTM
              WHEN 1
                 MOVE 'IE'                   TO 475X-E-FLAG-FILTRO
              WHEN 2
                 MOVE 'EE'                   TO 475X-E-FLAG-FILTRO
              WHEN 3
                 MOVE 'CE'                   TO 475X-E-FLAG-FILTRO
           END-EVALUATE

           MOVE 237                    TO 475X-E-CBCO.
           MOVE WRK-COD-USER           TO 475X-E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 475X-E-CTERM

           IF  WRK-INP-TPLI-EJTM         EQUAL '1'
               MOVE   'DCOM4752'         TO          WRK-MODULO
               MOVE WRK-INP-LINHA1-EJTM  TO WRK-LINHA-CPF-CNPJ
               MOVE WRK-NUMERO           TO 475X-E-CCNPJ-CPF
               MOVE WRK-FILIAL           TO 475X-E-CFLIAL-CNPJ
               MOVE WRK-CONTROLE         TO 475X-E-CCTRL-CNPJ-CPF
           ELSE
               MOVE   'DCOM4751'         TO          WRK-MODULO
               MOVE WRK-INP-LINHA2-EJTM  TO WRK-LINHA-AGENCIA
               MOVE WRK-AGENCIA-NUM      TO 475X-E-CAG-BCRIA
               MOVE WRK-CONTA            TO 475X-E-CCTA-BCRIA-CLI
           END-IF.

           CALL    WRK-MODULO          USING       475XE-ROTEADOR
                                                   475XS-ROTEADOR
                                                   ERRO-AREA
                                                   WRK-SQLCA.

           EVALUATE  475X-S-COD-RETORNO

             WHEN '0000'
             WHEN '0002'
                   IF   475X-S-CCONTR(10)          EQUAL ZEROS AND
                        475X-S-FLAG                EQUAL 'S'
                        MOVE 475X-S-FLAG      TO   WRK-INP-FIMP-EJTM
                                                   WRK-OUT-FIMP-EJTM
                   ELSE
                        MOVE SPACES           TO   WRK-INP-FIMP-EJTM
                                                   WRK-OUT-FIMP-EJTM
                   END-IF

             WHEN '0003'
                   MOVE '*'            TO WRK-INCONSIS
                   IF  COMU-PAGINA1            EQUAL 1 AND
                       COMU-OCCUR              EQUAL 1 AND
                       COMU-ACAO               EQUAL 'S'

                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-EJTM
                   ELSE
                       MOVE WRK-MSG01          TO  COMU-MENSAGEM
                   END-IF
                   PERFORM             0520-00-RETORNAR-CHAMADOR

             WHEN '0099'
                   MOVE  'DB2'         TO          ERR-TIPO-ACESSO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO

             WHEN  OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   IF  COMU-PAGINA1            EQUAL 1 AND
                       COMU-OCCUR              EQUAL 1 AND
                       COMU-ACAO               EQUAL 'S'

                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-EJTM
                   ELSE
                       MOVE 475X-S-MSG-RETORNO TO  COMU-MENSAGEM
                   END-IF
                   PERFORM             0520-00-RETORNAR-CHAMADOR

           END-EVALUATE.

      *----------------------------------------------------------------*
       0210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA DCOM6416 (NOME AGENCIA) *
      ******************************************************************
       0230-00-CH-DCOM6416             SECTION.
      *----------------------------------------------------------------*

           INITIALIZE  ENTRADA-ROTEADOR-6416.

           MOVE WRK-COD-USER           TO ROTENT-CFUNC-BDSCO-6416
           MOVE LNK-IO-LTERM           TO ROTENT-CTERM-6416
           MOVE 237                    TO ROTENT-BANCO-6416
           MOVE WRK-AGENCIA-NUM        TO ROTENT-DEPDC-6416
           MOVE 'N'                    TO ROTENT-FLAG-6416

           MOVE 'DCOM6416'             TO WRK-MODULO
           CALL WRK-MODULO             USING ENTRADA-ROTEADOR-6416
                                             SAIDA-ROTEADOR-6416
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE ROTSAI-COD-RETORNO-6416
              WHEN '0000'
                   MOVE ROTSAI-NOME-DEPDC-6416
                                            TO WRK-NOME-AGENCIA

               WHEN '0099'
                    MOVE 'DB2'               TO ERR-TIPO-ACESSO
                    PERFORM 9995-00-PROCESSAR-ROTINA-ERRO

               WHEN OTHER
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE ROTSAI-MSG-RETORNO-6416
                                       TO COMU-MENSAGEM
                   PERFORM             0520-00-RETORNAR-CHAMADOR

            END-EVALUATE.

      *----------------------------------------------------------------*
       0230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION PARA MONTAGEM DE OCORRENCIAS NA TELA                *
      ******************************************************************
       3000-00-MONTA-TELA              SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 10
             IF   475X-S-CCONTR(WRK-IND)  GREATER ZEROS
                  MOVE  WRK-193       TO      WRK-OUT-SEL-ATTR(WRK-IND)
                  MOVE 475X-S-CCONTR(WRK-IND)
                       TO            WRK-OUT-NUME-EJTM(WRK-IND)
                  MOVE 475X-S-IPSSOA-DESC-COML(WRK-IND)
                       TO            WRK-OUT-NOME-EJTM(WRK-IND)
                  MOVE 475X-S-CVRSAO-CONTR-LIM(WRK-IND)
                       TO            WRK-OUT-VERS-EJTM(WRK-IND)
                  MOVE 475X-S-DINIC-VGCIA-CONTR(WRK-IND)
                       TO            WRK-OUT-DTIN-EJTM(WRK-IND)
                  MOVE 475X-S-DVCTO-CONTR-LIM(WRK-IND)
                       TO            WRK-OUT-DTVC-EJTM(WRK-IND)
                  MOVE 475X-S-VCONTR-LIM-DESC(WRK-IND)
                       TO            WRK-OUT-VLIM-EDIT(WRK-IND)
                  MOVE 475X-S-CSGL-SIT-DESC(WRK-IND)
                       TO            WRK-OUT-SITU-EJTM(WRK-IND)
                  INSPECT WRK-OUT-DTIN-EJTM(WRK-IND)
                          REPLACING ALL '.' BY '/'
                  INSPECT WRK-OUT-DTVC-EJTM(WRK-IND)
                          REPLACING ALL '.' BY '/'
             ELSE
                  MOVE  WRK-225       TO      WRK-OUT-SEL-ATTR(WRK-IND)
                  MOVE SPACES    TO  WRK-OUT-NUME-EJTM(WRK-IND)
                                     WRK-OUT-VERS-EJTM(WRK-IND)
                                     WRK-OUT-NOME-EJTM(WRK-IND)
                                     WRK-OUT-DTIN-EJTM(WRK-IND)
                                     WRK-OUT-DTVC-EJTM(WRK-IND)
                                     WRK-OUT-VLIM-EJTM(WRK-IND)
                                     WRK-OUT-SITU-EJTM(WRK-IND)
             END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RECEBIMENTO DE MENSAGEM DO PROGRAMA      *
      ******************************************************************
       0500-00-PROCESSAR-DCOM0510      SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM        TO          WRK-INPUT-EJTM.
           PERFORM 0510-00-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EJTM     EQUAL 'H' OR
                   (WRK-INP-PFK-EJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EJTM EQUAL 'PFK01')
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-INP-MSGEM-EJTM     TO WRK-OUT-MSGEM-EJTM
                    IF  WRK-OUT-NUME-EJTM(1)    NOT EQUAL ZEROS
                        MOVE WRK-49345 TO WRK-OUT-SEL-ATTR(1)
                    END-IF

               WHEN WRK-INP-PFK-EJTM     EQUAL '3' OR
                   (WRK-INP-PFK-EJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EJTM EQUAL 'PFK03')
                    PERFORM 0520-00-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EJTM     EQUAL 'X' OR
                   (WRK-INP-PFK-EJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EJTM EQUAL 'PFK05')
                    PERFORM 0530-00-MENU-ROTINAS

               WHEN WRK-INP-PFK-EJTM     EQUAL 'V' OR
                   (WRK-INP-PFK-EJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EJTM EQUAL 'PFK10')
                    PERFORM 0540-00-MENU-DCOM

               WHEN WRK-INP-PFK-EJTM     EQUAL '7' OR
                   (WRK-INP-PFK-EJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EJTM EQUAL 'PFK07')
                    PERFORM 0550-00-TELA-ANTERIOR

               WHEN WRK-INP-PFK-EJTM     EQUAL '8' OR
                   (WRK-INP-PFK-EJTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EJTM EQUAL 'PFK08')
                    PERFORM 0560-00-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-EJTM          EQUAL '.' AND
                     WRK-INP-COMANDO-EJTM      EQUAL SPACES)
                    PERFORM 0511-00-DEVOLVER-OCORR
                    PERFORM 0570-00-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-49345         TO WRK-OUT-SEL-ATTR(1)
                    MOVE 'PF INVALIDA'     TO WRK-OUT-MSGEM-EJTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0510-00-DEVOLVER-TELA           SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-INP-ESCOND-EJTM   TO        WRK-OUT-ESCOND-EJTM.
           MOVE    WRK-INP-PAG-EJTM      TO        WRK-OUT-PAG-EJTM.
           MOVE    WRK-INP-TIMEST-EJTM   TO        WRK-OUT-TIMEST-EJTM.
           MOVE    WRK-INP-LINHA1-EJTM   TO        WRK-OUT-LINHA1-EJTM.
           MOVE    WRK-INP-LINHA2-EJTM   TO        WRK-OUT-LINHA2-EJTM.

      *----------------------------------------------------------------*
       0510-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0511-00-DEVOLVER-OCORR          SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 10

               IF  WRK-INP-NUME-EJTM(WRK-IND)    NOT EQUAL SPACES
                   MOVE  WRK-INP-SEL-EJTM(WRK-IND)
                                          TO  WRK-OUT-SEL-EJTM(WRK-IND)
                   MOVE  WRK-INP-NUME-EJTM(WRK-IND)
                                          TO  WRK-OUT-NUME-EJTM(WRK-IND)
                   MOVE  WRK-INP-VERS-EJTM(WRK-IND)
                                          TO  WRK-OUT-VERS-EJTM(WRK-IND)
                   MOVE  WRK-INP-NOME-EJTM(WRK-IND)
                                          TO  WRK-OUT-NOME-EJTM(WRK-IND)
                   MOVE  WRK-INP-DTIN-EJTM(WRK-IND)
                                          TO  WRK-OUT-DTIN-EJTM(WRK-IND)
                   MOVE  WRK-INP-DTVC-EJTM(WRK-IND)
                                          TO  WRK-OUT-DTVC-EJTM(WRK-IND)
                   MOVE  WRK-INP-VLIM-EJTM(WRK-IND)
                                          TO  WRK-OUT-VLIM-EJTM(WRK-IND)
                   MOVE  WRK-INP-SITU-EJTM(WRK-IND)
                                          TO  WRK-OUT-SITU-EJTM(WRK-IND)
               ELSE
                   MOVE  WRK-225          TO  WRK-OUT-SEL-ATTR(WRK-IND)
               END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       0511-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO AO PROGRAMA CHAMADOR             *
      ******************************************************************
       0520-00-RETORNAR-CHAMADOR       SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-INP-TIMEST-EJTM TO          COMU-TIMESTAMP.
           MOVE   'DCOM0510'           TO          COMU-TRANCODE.
           MOVE    LENGTH              OF          COMU-AREA
                                       TO          COMU-LL.
           MOVE   'DCOM0508'           TO          WRK-TELA.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.
           MOVE    COMU-AREA           TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       0520-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO AO MENU ROTINAS DO SENHAS        *
      ******************************************************************
       0530-00-MENU-ROTINAS            SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE  LENGTH OF COMU-AREA   TO          COMU-LL.
           MOVE   'DCOM0510'           TO          COMU-TRANCODE
                                                   WRK-TELA.
           MOVE   '5'                  TO          COMU-PFK.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.
           MOVE    COMU-AREA           TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       0530-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO AO MENU SISTEMA DCOM             *
      ******************************************************************
       0540-00-MENU-DCOM               SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE LENGTH OF COMU-AREA    TO          COMU-LL.
           MOVE   'DCOM0510'           TO          COMU-TRANCODE
                                                   WRK-TELA.
           MOVE   'A'                  TO          COMU-PFK.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.
           MOVE    COMU-AREA           TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       0540-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RETORNO DE TELA NA PAGINACAO             *
      ******************************************************************
       0550-00-TELA-ANTERIOR           SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-PAG-EJTM-N      EQUAL 1
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MSGEM-EJTM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0550-99-FIM
           END-IF.

           COMPUTE 475X-E-RESTART      = (WRK-INP-PAG-EJTM-N - 2) * 10

           PERFORM 0210-00-CH-DCOM475X.
           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0550-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.
           MOVE SG-MENSAGEM(01)        TO WRK-OUT-MSGEM-EJTM
           COMPUTE WRK-OUT-PAG-EJTM-N = WRK-INP-PAG-EJTM-N - 1.

      *----------------------------------------------------------------*
       0550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO AVANCO DE TELA NA PAGINACAO              *
      ******************************************************************
       0560-00-TELA-POSTERIOR          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMP-EJTM       EQUAL 'S'
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MSGEM-EJTM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0560-99-FIM
           END-IF.

           COMPUTE 475X-E-RESTART       = (WRK-INP-PAG-EJTM-N * 10)

           PERFORM 0210-00-CH-DCOM475X.
           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0560-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           IF   WRK-INP-FIMP-EJTM       EQUAL 'S'
                MOVE SG-MENSAGEM(17)    TO WRK-OUT-MSGEM-EJTM
           ELSE
                MOVE SG-MENSAGEM(01)    TO WRK-OUT-MSGEM-EJTM
           END-IF

           COMPUTE WRK-OUT-PAG-EJTM-N   = WRK-INP-PAG-EJTM-N + 1.

      *----------------------------------------------------------------*
       0560-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO PROCESSAMENTO PROGRAMA ( ENTER )         *
      ******************************************************************
       0570-00-TRATAR-ENTER            SECTION.
      *----------------------------------------------------------------*

           PERFORM 0571-00-CONSISTENCIAS.

           IF      WRK-INCONSIS        EQUAL SPACES
                   PERFORM             0572-00-TRANSFERIR-CONTROLE
           END-IF.

      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    ROTINA PARA CONSISTENCIA DOS ARGUMENTOS DE PESQUISA         *
      ******************************************************************
       0571-00-CONSISTENCIAS           SECTION.
      *----------------------------------------------------------------*

           MOVE    ZEROS               TO          WRK-COUNT.
           MOVE    SPACES              TO          WRK-INCONSIS.

           PERFORM VARYING WRK-IND     FROM 1 BY 1
                     UNTIL WRK-IND     GREATER 10

             IF    WRK-INP-SEL-EJTM(WRK-IND)
                                       NOT EQUAL   SPACES AND LOW-VALUES

                   MOVE  WRK-49353     TO  WRK-OUT-SEL-ATTR(WRK-IND)
               IF  WRK-INP-SEL-EJTM(WRK-IND)       EQUAL  'X'
                   ADD   1             TO          WRK-COUNT
                   MOVE  WRK-IND       TO          WRK-INDT
               ELSE
                   MOVE '*'            TO          WRK-INCONSIS
               END-IF
             END-IF

           END-PERFORM.

           IF      WRK-INCONSIS        EQUAL       '*'
                   MOVE SG-MENSAGEM(086)   TO      WRK-OUT-MSGEM-EJTM
           ELSE
            IF     WRK-COUNT           EQUAL       0
                   MOVE  '*'           TO          WRK-INCONSIS
                   MOVE SG-MENSAGEM(0217)  TO      WRK-OUT-MSGEM-EJTM
            ELSE
             IF    WRK-COUNT           GREATER     1
                   MOVE  '*'           TO          WRK-INCONSIS
                   MOVE SG-MENSAGEM(0215)  TO      WRK-OUT-MSGEM-EJTM
             END-IF
            END-IF
           END-IF.

      *----------------------------------------------------------------*
       1010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO CONTROLE TRANSFERENCIA OUTROS PROGRAMAS  *
      ******************************************************************
       0572-00-TRANSFERIR-CONTROLE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE    WRK-INP-OPCAO-EJTM  TO          COMU-TPOPCAO.
           MOVE    WRK-INP-TIMEST-EJTM TO          COMU-TIMESTAMP.
           MOVE   'DCOM0510'           TO          COMU-TRANCODE.

           MOVE    LENGTH              OF          COMU-AREA
                                       TO          COMU-LL.
           MOVE    WRK-INP-NUME-EJTM(WRK-INDT) TO  COMU-CONTRATO-LIM
           MOVE    WRK-INP-VERS-EJTM(WRK-INDT) TO  COMU-VERSAO
           MOVE    WRK-INP-PAG-EJTM-N          TO  COMU-PAGINA1
           MOVE    237                         TO  COMU-BANCO

           IF      WRK-INP-PAG-EJTM-N          EQUAL 1  AND
                   WRK-INP-NUME-EJTM(2)        NOT NUMERIC
                   MOVE 1                      TO  COMU-OCCUR
           END-IF

           IF      WRK-INP-TPLI-EJTM   EQUAL       1
                   MOVE WRK-INP-LINHA1-EJTM    TO  WRK-LINHA-CPF-CNPJ
                   MOVE WRK-NUMERO(1:3)        TO  COMU-CNPJCPF(1:3)
                   MOVE WRK-NUMERO(5:3)        TO  COMU-CNPJCPF(4:3)
                   MOVE WRK-NUMERO(9:3)        TO  COMU-CNPJCPF(7:3)
                   MOVE WRK-FILIAL             TO  COMU-FILIAL
                   MOVE WRK-CONTROLE           TO  COMU-CONTROLE
           ELSE
                   MOVE WRK-INP-LINHA2-EJTM    TO  WRK-LINHA-AGENCIA
                   MOVE WRK-AGENCIA-NUM        TO  COMU-AGENCIA
                   MOVE WRK-CONTA              TO  COMU-CONTA
           END-IF

           EVALUATE  WRK-INP-OPCAO-EJTM
               WHEN  1
                   MOVE 'DCOM0511'     TO          WRK-TELA
               WHEN  2
                   MOVE 'DCOM0512'     TO          WRK-TELA
               WHEN  3
                   MOVE 'DCOM0513'     TO          WRK-TELA
               WHEN  OTHER
                   MOVE 'OPCAO INVALIDA '
                                       TO          WRK-OUT-MSGEM-EJTM
           END-EVALUATE.

           MOVE    COMU-AREA           TO          WRK-MENSAGEM.
           MOVE    WRK-CHNG            TO          WRK-FUNCAO.

      *----------------------------------------------------------------*
       0572-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA BRAD0660 (TELA MFS)     *
      ******************************************************************
       9500-00-PROCESSAR-BRAD0660      SECTION.
      *----------------------------------------------------------------*

           MOVE LENGTH OF WRK-OUTPUT-EJTM
                                       TO  WRK-660-LL-MENSAGEM-EJTM
                                           WRK-OUT-LL-EJTM

           MOVE LENGTH OF WRK-660-DCOMEJTM
                                       TO  WRK-660-LL-AREA-EJTM

           CALL   'BRAD0660'           USING       WRK-OUTPUT-EJTM
                                                   WRK-660-DCOMEJTM.

           IF      RETURN-CODE         NOT EQUAL   ZEROS
                   MOVE 'APL'          TO          ERR-TIPO-ACESSO
                   MOVE  RETURN-CODE   TO          WRK-RETURN-CODE
                   MOVE  20            TO          WRK-LOCAL-ERRO
                   MOVE  WRK-ERRO-BRAD0660 TO      ERR-TEXTO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE    WRK-ISRT            TO          WRK-FUNCAO.
           MOVE    WRK-OUTPUT-EJTM     TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      **===============================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA BRAD7100 (ABENDS)       *
      ******************************************************************
       9995-00-PROCESSAR-ROTINA-ERRO   SECTION.
      *----------------------------------------------------------------*

           IF      ERR-MODULO          NOT EQUAL   SPACES AND LOW-VALUES
                   MOVE 'DCOM0510'     TO          ERR-PGM
           ELSE
                   MOVE 'SENHAS02'     TO          ERR-PGM
                   MOVE 'DCOM0510'     TO          ERR-MODULO
           END-IF.

           MOVE    WRK-COD-USER        TO          ERR-COD-USER.
           MOVE    WRK-COD-DEPTO       TO          ERR-COD-DEPTO.

           IF      ERR-TIPO-ACESSO     EQUAL      'DB2'
                   CALL 'BRAD7100'     USING       LNK-IO-PCB
                                                   ERRO-AREA
                                                   LNK-ALT-PCB
                                                   WRK-SQLCA
           ELSE
                   CALL 'BRAD7100'     USING       LNK-IO-PCB
                                                   ERRO-AREA
                                                   LNK-ALT-PCB
           END-IF.

           PERFORM 9999-00-FINALIZAR.

      *----------------------------------------------------------------*
       9995-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       9999-00-FINALIZAR               SECTION.
      *----------------------------------------------------------------*

           GOBACK.

      *----------------------------------------------------------------*
       9999-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
