      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. DCOM0504.
       AUTHOR.     ALTRAN.
      *================================================================*
      *                        A L T R A N                             *
      *----------------------------------------------------------------*
      *    PROGRAMA....:   DCOM0504                                    *
      *    PROGRAMADOR.:   FSW - ALCIONE                               *
      *    ANALISTA ...:   EMERSON ARTUR FRANCO                        *
      *    ANALISTA.BCO:   EDGARD RODRIGUES                            *
      *    DATA........:   22/04/2007                                  *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   ESTORNO DE OPERACAO - OPERACOES POR CLIENTE *
      *----------------------------------------------------------------*
      *    TELA........:   DCOMEDTM - ESTORNO DA OPERACAO POR AGENCIA  *
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
      *               DCOM0502                       DCOM0505          *
      *                  -                           DCOM0506          *
      *                  -                           DCOM0507          *
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
            'NAO FORAM ENCONTRADOS DADOS PARA ESTE ARGUMENTO DE PESQUISA
      -     ' '.
      *
      *----------------------------------------------------------------*
      *--- ATRIBUTOS PARA VARIAVEIS DE TELA                        ----*
      *--- ALF - NOR  -DESP - DISP                       (+193)    ----*
      *--- ALF - BRI - DESP - DISP                       (+201)    ----*
      *--- ALF - NOR - PROT - DISP                       (+225)    ----*
      *--- ALF - NOR - DESP - DISP - CURSOR              (+49353)  ----*
      *--- ALF - BRI - DESP - DISP - CURSOR              (+49369)  ----*
      *----------------------------------------------------------------*
       01  WRK-193                     PIC S9(008)  COMP VALUE +193.
       01  WRK-201                     PIC S9(008)  COMP VALUE +201.
       01  WRK-225                     PIC S9(008)  COMP VALUE +225.
       01  WRK-49353                   PIC S9(008)  COMP VALUE +49353.
       01  WRK-49345                   PIC S9(008)  COMP VALUE +49345.
       01  WRK-49369                   PIC S9(008)  COMP VALUE +49369.
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
           05  WRK-FILLER              PIC  X(010)  VALUE SPACES.
           05  WRK-LT-CNPJCPF          PIC  X(010)  VALUE 'CNPJ/CPF: '.
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
           '* AREA ROTINA DCOM472X *'.
      *----------------------------------------------------------------*
      *
       01  472XE-ROTEADOR.
           03  472X-E-HEADER.
               05  472X-E-COD-RETORNO             PIC  X(004).
               05  472X-E-MSG-RETORNO             PIC  X(079).
               05  472X-E-RESTART                 PIC  9(005).
               05  472X-E-FLAG                    PIC  X(001).
               05  472X-E-START-COUNT             PIC  X(001).
               05  472X-E-CFUNC-BDSCO             PIC  9(009).
               05  472X-E-CTERM                   PIC  X(008).
           03  472X-E-DADOS.
               10  472X-E-FLAG-FILTRO             PIC  X(002).
               10  472X-E-CBCO                    PIC  9(003).
               10  472X-E-CAG-BCRIA               PIC  9(005).
               10  472X-E-CAG-OPER                PIC  9(005).
               10  472X-E-CCTA-BCRIA-CLI          PIC  9(013).
               10  472X-E-CPRODT                  PIC  9(003).
               10  472X-E-CSPROD-DESC-COML        PIC  9(003).
               10  472X-E-DINIC-OPER-DESC         PIC  X(010).
               10  472X-E-CELMTO-DESC-COML        PIC  9(003).
FS2511*        10  472X-E-CCNPJ-CPF               PIC  9(009).
FS2511         10  472X-E-CCNPJ-CPF               PIC  X(009).
FS2511*        10  472X-E-CFLIAL-CNPJ             PIC  9(005).
FS2511         10  472X-E-CFLIAL-CNPJ             PIC  X(004).
               10  472X-E-CCTRL-CNPJ-CPF          PIC  9(002).
               10  472X-E-CSIT-DESC-COML          PIC  9(003).
               10  472X-E-CINDCD-EVNTO-WORKFLOW   PIC  9(003).
               10  472X-E-CINDCD-SIT-FINAL-EVNTO  PIC  9(003).

       01  472XS-ROTEADOR.
           03  472X-S-HEADER.
               05  472X-S-COD-RETORNO      PIC  X(004).
               05  472X-S-MSG-RETORNO      PIC  X(079).
               05  472X-S-RESTART          PIC  9(005).
               05  472X-S-FLAG             PIC  X(001).
               05  472X-S-QTDE-RETORNADA   PIC  9(003).
               05  472X-S-COUNT            PIC  9(009).

           03  472X-S-DADOS.
               05  472X-S-TABELA-SAIDA     OCCURS 10  TIMES.
                   10  472X-S-DANO-OPER-DESC        PIC  9(004).
                   10  472X-S-NSEQ-OPER-DESC        PIC  9(009).
                   10  472X-S-IPSSOA-DESC-COML      PIC  X(060).
                   10  472X-S-CBCO                  PIC  9(003).
                   10  472X-S-CAG-BCRIA             PIC  9(005).
                   10  472X-S-CAG-BCRIA-DESC        PIC  X(040).
                   10  472X-S-CCTA-BCRIA-CLI        PIC  9(013).
                   10  472X-S-CBCO-OPER-OPER        PIC  9(003).
                   10  472X-S-CAG-OPER-OPER         PIC  9(005).
                   10  472X-S-CSIT-DESC-COML        PIC  9(003).
                   10  472X-S-IRSUMO-SIT-DESC       PIC  X(015).
                   10  472X-S-CSGL-SIT-DESC         PIC  X(006).
                   10  472X-S-CPRODT                PIC  9(003).
                   10  472X-S-IABREV-PRODT          PIC  X(012).
                   10  472X-S-CSGL-PRODT-DESC       PIC  X(006).
                   10  472X-S-CSPROD-DESC-COML      PIC  9(003).
                   10  472X-S-CTPO-DESC-COML        PIC  9(003).
                   10  472X-S-IRSUMO-SPROD-DESC     PIC  X(010).
                   10  472X-S-CSGL-SPROD-DESC       PIC  X(006).
                   10  472X-S-VOPER-DESC-COML       PIC  9(015)V9(2).
                   10  472X-S-DINIC-OPER-DESC       PIC  X(010).
                   10  472X-S-DVCTO-FNAL-OPER       PIC  X(010).
                   10  472X-S-CMEIO-ENTRD-DESC      PIC  9(03).
                   10  472X-S-IMEIO-ENTRD-DESC      PIC  X(25).
                   10  472X-S-IRSUMO-MEIO-ENTRD     PIC  X(15).
                   10  472X-S-CSGL-MEIO-ENTRD       PIC  X(06).
                   10  472X-S-CINDCD-LIBRC-ANTCP    PIC  X(01).
                   10  472X-S-PARECER-FORML         PIC  X(01).
                   10  472X-S-VLIQ-OPER-DESC        PIC  9(015)V9(2).

      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA ROTINA DCOM5335 *'.
      *----------------------------------------------------------------*

       01  5335-ENTRADA.
           05 5335E-COD-RETORNO        PIC  X(004)         VALUE SPACES.
           05 5335E-MSG-RETORNO        PIC  X(079)         VALUE SPACES.
           05 5335E-RESTART            PIC  9(005)         VALUE ZEROS.
           05 5335E-FLAG               PIC  9(001)         VALUE ZEROS.
           05 5335E-CFUNC-BDSCO        PIC  9(009)         VALUE ZEROS.
           05 5335E-CTERM              PIC  X(008)         VALUE SPACES.
           05 5335E-OPERACAO.
              10 5335E-DANO-OPER-DESC  PIC  9(004)         VALUE ZEROS.
              10 5335E-NSEQ-OPER-DESC  PIC  9(009)         VALUE ZEROS.
           05 5335E-NPCELA-DESC-COML   PIC  9(005)         VALUE ZEROS.
           05 5335E-CELMTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-CEVNTO-DESC-COML   PIC  9(003)         VALUE ZEROS.
           05 5335E-ACAO               PIC  X(001)         VALUE SPACES.

       01  5335-SAIDA.
           05 5335S-COD-RETORNO        PIC  9(004)         VALUE ZEROS.
           05 5335S-MSG-RETORNO        PIC  X(079)         VALUE SPACES.

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
               10  COMU-PAGINA1        PIC  9(004)         VALUE ZEROS.
               10  COMU-OCCUR          PIC  9(008)         VALUE ZEROS.
           05  COMU-MENSAGEM           PIC  X(079)         VALUE SPACES.
           05  COMU-ACAO               PIC  X(001)         VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA INPUT DCOMEDTM **'.
      *----------------------------------------------------------------*
       01  WRK-INPUT-EDTM.
           03  WRK-INP-TESTE                  PIC  X(018) VALUE SPACES.
           03  WRK-INP-PFK-EDTM               PIC  X(001) VALUE SPACES.
           03  WRK-INP-DADOS-EDTM.
               05  WRK-INP-SENHAS-EDTM        PIC  X(037) VALUE SPACES.
               05  WRK-INP-COMANDO-EDTM       PIC  X(068) VALUE SPACES.
               05  WRK-INP-ESCOND-EDTM.
                   10  WRK-INP-OPCAO-EDTM     PIC  9(001) VALUE ZEROS.
                   10  WRK-INP-CPAG-EDTM      PIC  9(003) VALUE ZEROS.
                   10  WRK-INP-QTIP-EDTM      PIC  9(001) VALUE ZEROS.
                   10  WRK-INP-FIMP-EDTM      PIC  X(001) VALUE SPACES.
                   10  WRK-INP-TPLI-EDTM      PIC  X(001) VALUE SPACES.
                   10  WRK-INP-VADO-EDTM      PIC  X(023) VALUE SPACES.
               05  WRK-INP-TIMEST-EDTM        PIC  X(026) VALUE SPACES.
               05  WRK-INP-PAG-EDTM.
                   10  WRK-INP-PAG-EDTM-N     PIC  9(003) VALUE ZEROS.
               05  WRK-INP-LINHA1-EDTM        PIC  X(079) VALUE SPACES.
               05  WRK-INP-LINHA2-EDTM        PIC  X(079) VALUE SPACES.
               05  WRK-INP-TAB-EDTM   OCCURS 10  TIMES.
                   10  WRK-INP-SEL-EDTM       PIC  X(001).
                   10  WRK-INP-PROD-EDTM.
                       15  WRK-INP-CDPROD-EDTM     PIC  9(003).
                       15  WRK-INP-HIFEM1-EDTM     PIC  X(001).
                       15  WRK-INP-DSPROD-EDTM     PIC  X(012).
                   10  WRK-INP-SUBP-EDTM.
                       15  WRK-INP-CDSUBP-EDTM     PIC  9(003).
                       15  WRK-INP-HIFEM2-EDTM     PIC  X(001).
                       15  WRK-INP-DSSUBP-EDTM     PIC  X(010).
                   10  WRK-INP-OPER-EDTM      PIC  X(013).
                   10  WRK-INP-DTIN-EDTM      PIC  X(010).
                   10  WRK-INP-SITU-EDTM      PIC  X(020).
               05  WRK-INP-MSGEM-EDTM         PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA OUTPUT DCOMEDTM *'.
      *----------------------------------------------------------------*
       01  WRK-OUTPUT-EDTM.
           03  WRK-OUT-LL-EDTM         PIC S9(004)  COMP VALUE +0000.
           03  WRK-OUT-ZZ-EDTM         PIC S9(004)  COMP VALUE ZEROS.
           03  WRK-OUT-DADOS-EDTM.
               05  WRK-OUT-SENHAS-EDTM  PIC  X(037) VALUE SPACES.
               05  WRK-OUT-COMANDO-EDTM PIC  X(068) VALUE SPACES.
               05  WRK-OUT-ESCOND-EDTM.
                   10  WRK-OUT-OPCAO-EDTM     PIC  9(001) VALUE ZEROS.
                   10  WRK-OUT-CPAG-EDTM      PIC  9(003) VALUE ZEROS.
                   10  WRK-OUT-QTIP-EDTM      PIC  9(001) VALUE ZEROS.
                   10  WRK-OUT-FIMP-EDTM      PIC  X(001) VALUE SPACES.
                   10  WRK-OUT-TPLI-EDTM      PIC  X(001) VALUE SPACES.
                   10  WRK-OUT-VAGO-EDTM      PIC  X(023) VALUE SPACES.
               05  WRK-OUT-TIMEST-EDTM        PIC  X(026) VALUE SPACES.
               05  WRK-OUT-PAG-EDTM.
                   10  WRK-OUT-PAG-EDTM-N     PIC  9(003) VALUE ZEROS.
               05  WRK-OUT-LINHA1-EDTM        PIC  X(079) VALUE SPACES.
               05  WRK-OUT-LINHA2-EDTM        PIC  X(079) VALUE SPACES.
               05  WRK-OUT-TAB-EDTM   OCCURS 10  TIMES.
                   10  WRK-OUT-SEL-ATTR       PIC S9(004) COMP.
                   10  WRK-OUT-SEL-EDTM       PIC  X(001).
                   10  WRK-OUT-PROD-EDTM.
                       15  WRK-OUT-CDPROD-EDTM     PIC  9(003).
                       15  WRK-OUT-HIFEM1-EDTM     PIC  X(001).
                       15  WRK-OUT-DSPROD-EDTM     PIC  X(012).
                   10  WRK-OUT-SUBP-EDTM.
                       15  WRK-OUT-CDSUBP-EDTM     PIC  9(003).
                       15  WRK-OUT-HIFEM2-EDTM     PIC  X(001).
                       15  WRK-OUT-DSSUBP-EDTM     PIC  X(010).
                   10  WRK-OUT-OPER-EDTM      PIC  X(013).
                   10  WRK-OUT-DTIN-EDTM      PIC  X(010).
                   10  WRK-OUT-SITU-EDTM      PIC  X(020).
               05  WRK-OUT-MSGEM-EDTM         PIC  X(079) VALUE SPACES.
      *
      *================================================================*
       01  FILLER                      PIC  X(024)  VALUE
           '* AREA BRAD0660 (EDTM) *'.
      *----------------------------------------------------------------*
       01  WRK-660-DCOMEDTM.
           05  WRK-660-LL-AREA-EDTM       PIC  9(004) COMP VALUE  0000.
           05  WRK-660-LL-MENSAGEM-EDTM   PIC  9(004) COMP VALUE  0000.
           05  WRK-660-SENHAS-EDTM        PIC  9(004) COMP VALUE  0037.
           05  WRK-660-COMANDO-EDTM       PIC  9(004) COMP VALUE  1068.
           05  WRK-660-ESCOND-EDTM        PIC  9(004) COMP VALUE  0030.
           05  WRK-660-TIM-EDTM           PIC  9(004) COMP VALUE  0026.
           05  WRK-660-PAG-EDTM           PIC  9(004) COMP VALUE  0003.
           05  WRK-660-LINHA1-EDTM        PIC  9(004) COMP VALUE  0079.
           05  WRK-660-LINHA2-EDTM        PIC  9(004) COMP VALUE  0079.
           05  WRK-660-OCORRS-EDTM      OCCURS 10  TIMES.
             10  WRK-660-SEL-EDTM         PIC  9(004) COMP VALUE  2003.
             10  WRK-660-PROD-EDTM        PIC  9(004) COMP VALUE  0016.
             10  WRK-660-SUBP-EDTM        PIC  9(004) COMP VALUE  0014.
             10  WRK-660-OPER-EDTM        PIC  9(004) COMP VALUE  0013.
             10  WRK-660-DTIN-EDTM        PIC  9(004) COMP VALUE  0010.
             10  WRK-660-SITU-EDTM        PIC  9(004) COMP VALUE  0020.
           05  WRK-660-MSGEM-EDTM         PIC  9(004) COMP VALUE  0079.
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

           MOVE    SPACES              TO          WRK-OUT-DADOS-EDTM
                                                   WRK-INCONSIS.

           INITIALIZE                              472XE-ROTEADOR
                                                   472XS-ROTEADOR.

           MOVE    'DCOMEDTM'          TO          WRK-TELA.

           INITIALIZE          COMU-AREA.

           IF      WRK-TRANSACAO       NOT EQUAL  'DCOM0504'
                   PERFORM             0200-00-RECEBER-CONTROLE
           ELSE
                   PERFORM             0500-00-PROCESSAR-DCOM0504
           END-IF.

           IF      WRK-TELA            EQUAL      'DCOMEDTM'
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
           MOVE    COMU-TPOPCAO        TO        WRK-INP-OPCAO-EDTM.
           MOVE    COMU-TIMESTAMP      TO        WRK-OUT-TIMEST-EDTM
                                                 WRK-INP-TIMEST-EDTM.

           PERFORM 0201-00-ACESSAR-DCOM6408.

           IF  COMU-CNPJCPF            NOT EQUAL SPACES AND
               COMU-CNPJCPF NOT EQUAL LOW-VALUES
               MOVE '1'                TO WRK-INP-TPLI-EDTM
                                          WRK-OUT-TPLI-EDTM
               MOVE COMU-CNPJCPF       TO WRK-NUMERO
               MOVE COMU-FILIAL        TO WRK-FILIAL
               MOVE COMU-CONTROLE      TO WRK-CONTROLE
               MOVE 'CNPJ/CPF: '       TO WRK-LT-CNPJCPF
               MOVE WRK-LINHA-CPF-CNPJ TO WRK-OUT-LINHA1-EDTM
                                          WRK-INP-LINHA1-EDTM
               MOVE SPACES             TO WRK-LINHA-AGENCIA
               MOVE WRK-LINHA-AGENCIA  TO WRK-OUT-LINHA2-EDTM
                                          WRK-INP-LINHA2-EDTM
           ELSE
               MOVE '2'                TO WRK-INP-TPLI-EDTM
                                          WRK-OUT-TPLI-EDTM
               MOVE SPACES             TO WRK-CPF-CNPJ
                                          WRK-LT-CNPJCPF
               MOVE WRK-LINHA-CPF-CNPJ TO WRK-OUT-LINHA1-EDTM
                                          WRK-INP-LINHA1-EDTM
               MOVE COMU-AGENCIA       TO WRK-AGENCIA-NUM
               PERFORM 0230-00-CH-DCOM6416
               MOVE COMU-CONTA         TO WRK-CONTA
               MOVE WRK-LINHA-AGENCIA  TO WRK-OUT-LINHA2-EDTM
                                          WRK-INP-LINHA2-EDTM
           END-IF.

           IF  WRK-TRANSACAO           EQUAL   'DCOM0502'
               MOVE    1               TO          WRK-INP-PAG-EDTM-N
           ELSE
               IF  COMU-PAGINA1        GREATER 1
                   COMPUTE 472X-E-RESTART  = (COMU-PAGINA1 - 1) * 10
                   MOVE    COMU-PAGINA1    TO      WRK-INP-PAG-EDTM-N
               ELSE
                   MOVE    1               TO      WRK-INP-PAG-EDTM-N
               END-IF
           END-IF.

           PERFORM 0210-00-CH-DCOM472X.

           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0200-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           MOVE    WRK-INP-ESCOND-EDTM TO          WRK-OUT-ESCOND-EDTM.
           MOVE    WRK-INP-PAG-EDTM-N  TO          WRK-OUT-PAG-EDTM-N.

           IF  WRK-TRANSACAO               EQUAL     'DCOM0505' OR
                                                     'DCOM0506' OR
                                                     'DCOM0507'
               IF  COMU-MENSAGEM           NOT EQUAL  SPACES
                   MOVE    COMU-MENSAGEM          TO  WRK-OUT-MSGEM-EDTM
               ELSE
                   MOVE    SG-MENSAGEM(214)       TO  WRK-OUT-MSGEM-EDTM
               END-IF
           ELSE
               MOVE    SG-MENSAGEM(214)    TO  WRK-OUT-MSGEM-EDTM
           END-IF
           .
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

           IF  COMU-CNPJCPF            NOT EQUAL SPACES AND
               COMU-CNPJCPF            NOT EQUAL LOW-VALUES
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
      *    SECTION CHAMADA E TRATAMENTO ROTINA DCOM472X                *
      ******************************************************************
       0210-00-CH-DCOM472X             SECTION.
      *----------------------------------------------------------------*


           EVALUATE  WRK-INP-OPCAO-EDTM
              WHEN 1
                 MOVE 'IE'                   TO 472X-E-FLAG-FILTRO
              WHEN 2
                 MOVE 'EE'                   TO 472X-E-FLAG-FILTRO
              WHEN 3
                 MOVE 'CE'                   TO 472X-E-FLAG-FILTRO
           END-EVALUATE

           MOVE 237                    TO 472X-E-CBCO.
           MOVE WRK-COD-USER           TO 472X-E-CFUNC-BDSCO
           MOVE LNK-IO-LTERM           TO 472X-E-CTERM
           MOVE WRK-COD-DEPTO-N        TO 472X-E-CAG-OPER

           IF  WRK-INP-TPLI-EDTM         EQUAL '1'
               MOVE   'DCOM4722'         TO          WRK-MODULO
               MOVE WRK-INP-LINHA1-EDTM  TO WRK-LINHA-CPF-CNPJ
               MOVE WRK-NUMERO           TO 472X-E-CCNPJ-CPF
               MOVE WRK-FILIAL           TO 472X-E-CFLIAL-CNPJ
               MOVE WRK-CONTROLE         TO 472X-E-CCTRL-CNPJ-CPF
               MOVE '0'                  TO 472X-E-MSG-RETORNO(1:1)
               MOVE WRK-COD-DEPTO-N      TO 472X-E-CAG-OPER
                                            472X-E-MSG-RETORNO(2:4)
           ELSE
               MOVE   'DCOM4721'         TO          WRK-MODULO
               MOVE WRK-INP-LINHA2-EDTM  TO WRK-LINHA-AGENCIA
               MOVE WRK-COD-DEPTO-N      TO 472X-E-CAG-OPER
               MOVE WRK-AGENCIA-NUM      TO 472X-E-CAG-BCRIA
               MOVE WRK-CONTA            TO 472X-E-CCTA-BCRIA-CLI
           END-IF.

           CALL    WRK-MODULO          USING       472XE-ROTEADOR
                                                   472XS-ROTEADOR
                                                   ERRO-AREA
                                                   WRK-SQLCA.

           EVALUATE  472X-S-COD-RETORNO

             WHEN '0000'
             WHEN '0002'
                   IF 472X-S-DANO-OPER-DESC(6) EQUAL   ZEROS     AND
                      472X-S-FLAG              EQUAL 'S'
                      MOVE 472X-S-FLAG         TO WRK-INP-FIMP-EDTM
                                                  WRK-OUT-FIMP-EDTM
                   END-IF
             WHEN '0003'
                   MOVE '*'            TO WRK-INCONSIS
                   IF  COMU-PAGINA1            EQUAL 1 AND
                       COMU-OCCUR              EQUAL 1 AND
                       COMU-ACAO               EQUAL 'S'

                       MOVE COMU-MENSAGEM      TO  WRK-OUT-MSGEM-EDTM
                   ELSE
                       MOVE SG-MENSAGEM(0213)  TO  COMU-MENSAGEM
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

                       MOVE COMU-MENSAGEM        TO WRK-OUT-MSGEM-EDTM
                   ELSE
                       MOVE 472X-S-MSG-RETORNO   TO COMU-MENSAGEM
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
                   MOVE WRK-MSG01      TO COMU-MENSAGEM
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

             IF   472X-S-DANO-OPER-DESC(WRK-IND) GREATER ZEROS
                  MOVE  WRK-193       TO      WRK-OUT-SEL-ATTR(WRK-IND)
                  MOVE 472X-S-DANO-OPER-DESC(WRK-IND)
                       TO            WRK-ANO-OPER
                  MOVE 472X-S-NSEQ-OPER-DESC(WRK-IND)
                       TO            WRK-SEQ-OPER
                  MOVE WRK-OPERACAO
                       TO            WRK-OUT-OPER-EDTM(WRK-IND)
                  MOVE 472X-S-CPRODT(WRK-IND)
                       TO            WRK-OUT-CDPROD-EDTM(WRK-IND)
                  MOVE '-'
                       TO            WRK-OUT-HIFEM1-EDTM(WRK-IND)
                  MOVE 472X-S-IABREV-PRODT(WRK-IND)
                       TO            WRK-OUT-DSPROD-EDTM(WRK-IND)
                  MOVE 472X-S-CSPROD-DESC-COML(WRK-IND)
                       TO            WRK-OUT-CDSUBP-EDTM(WRK-IND)
                  MOVE '-'
                       TO            WRK-OUT-HIFEM2-EDTM(WRK-IND)
                  MOVE 472X-S-IRSUMO-SPROD-DESC(WRK-IND)
                       TO            WRK-OUT-DSSUBP-EDTM(WRK-IND)
                  MOVE 472X-S-DINIC-OPER-DESC(WRK-IND)
                       TO            WRK-OUT-DTIN-EDTM(WRK-IND)
                  INSPECT WRK-OUT-DTIN-EDTM(WRK-IND)
                          REPLACING ALL '.' BY '/'
                  MOVE 472X-S-IRSUMO-SIT-DESC(WRK-IND)
                       TO            WRK-OUT-SITU-EDTM(WRK-IND)
             ELSE
                  MOVE  WRK-225       TO      WRK-OUT-SEL-ATTR(WRK-IND)
                  MOVE SPACES    TO  WRK-OUT-PROD-EDTM(WRK-IND)
                                     WRK-OUT-SUBP-EDTM(WRK-IND)
                                     WRK-OUT-OPER-EDTM(WRK-IND)
                                     WRK-OUT-DTIN-EDTM(WRK-IND)
                                     WRK-OUT-SITU-EDTM(WRK-IND)
             END-IF

           END-PERFORM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO RECEBIMENTO DE MENSAGEM DO PROGRAMA      *
      ******************************************************************
       0500-00-PROCESSAR-DCOM0504      SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-MENSAGEM        TO          WRK-INPUT-EDTM.
           PERFORM 0510-00-DEVOLVER-TELA.

           EVALUATE TRUE
               WHEN WRK-INP-PFK-EDTM     EQUAL 'H' OR
                   (WRK-INP-PFK-EDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EDTM EQUAL 'PFK01')
                    MOVE 'H'             TO    WRK-INP-PFK-EDTM
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-INP-MSGEM-EDTM     TO WRK-OUT-MSGEM-EDTM
                    IF  WRK-OUT-OPER-EDTM(1)    NOT EQUAL ZEROS
                        MOVE WRK-49345 TO WRK-OUT-SEL-ATTR(1)
                    END-IF

               WHEN WRK-INP-PFK-EDTM     EQUAL '3' OR
                   (WRK-INP-PFK-EDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EDTM EQUAL 'PFK03')
                    PERFORM 0520-00-RETORNAR-CHAMADOR

               WHEN WRK-INP-PFK-EDTM     EQUAL 'X' OR
                   (WRK-INP-PFK-EDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EDTM EQUAL 'PFK05')
                    PERFORM 0530-00-MENU-ROTINAS

               WHEN WRK-INP-PFK-EDTM     EQUAL 'V' OR
                   (WRK-INP-PFK-EDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EDTM EQUAL 'PFK10')
                    PERFORM 0540-00-MENU-DCOM

               WHEN WRK-INP-PFK-EDTM     EQUAL '7' OR
                   (WRK-INP-PFK-EDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EDTM EQUAL 'PFK07')
                    PERFORM 0550-00-TELA-ANTERIOR

               WHEN WRK-INP-PFK-EDTM     EQUAL '8' OR
                   (WRK-INP-PFK-EDTM     EQUAL '.' AND
                    WRK-INP-COMANDO-EDTM EQUAL 'PFK08')
                    PERFORM 0560-00-TELA-POSTERIOR

               WHEN (WRK-INP-PFK-EDTM          EQUAL '.' AND
                     WRK-INP-COMANDO-EDTM      EQUAL SPACES)
                    PERFORM 0511-00-DEVOLVER-OCORR
                    PERFORM 0570-00-TRATAR-ENTER

               WHEN OTHER
                    PERFORM 0511-00-DEVOLVER-OCORR
                    MOVE WRK-49345         TO WRK-OUT-SEL-ATTR(1)
                    MOVE 'PF INVALIDA'     TO WRK-OUT-MSGEM-EDTM

           END-EVALUATE.

      *----------------------------------------------------------------*
       0500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO DE MENSAGEM DE ENTRADA E SAIDA TELA      *
      ******************************************************************
       0510-00-DEVOLVER-TELA           SECTION.
      *----------------------------------------------------------------*

           MOVE    WRK-INP-ESCOND-EDTM   TO        WRK-OUT-ESCOND-EDTM.
           MOVE    WRK-INP-PAG-EDTM      TO        WRK-OUT-PAG-EDTM.
           MOVE    WRK-INP-TIMEST-EDTM   TO        WRK-OUT-TIMEST-EDTM.
           MOVE    WRK-INP-LINHA1-EDTM   TO        WRK-OUT-LINHA1-EDTM.
           MOVE    WRK-INP-LINHA2-EDTM   TO        WRK-OUT-LINHA2-EDTM.

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

               IF  WRK-INP-OPER-EDTM(WRK-IND)    NOT EQUAL SPACES
                   MOVE  WRK-INP-SEL-EDTM(WRK-IND)
                     TO  WRK-OUT-SEL-EDTM(WRK-IND)
                   MOVE  WRK-INP-PROD-EDTM(WRK-IND)
                     TO  WRK-OUT-PROD-EDTM(WRK-IND)
                   MOVE  WRK-INP-SUBP-EDTM(WRK-IND)
                     TO  WRK-OUT-SUBP-EDTM(WRK-IND)
                   MOVE  WRK-INP-OPER-EDTM(WRK-IND)
                     TO  WRK-OUT-OPER-EDTM(WRK-IND)
                   MOVE  WRK-INP-DTIN-EDTM(WRK-IND)
                     TO  WRK-OUT-DTIN-EDTM(WRK-IND)
                   MOVE  WRK-INP-SITU-EDTM(WRK-IND)
                     TO  WRK-OUT-SITU-EDTM(WRK-IND)
               ELSE
                   MOVE  WRK-225       TO  WRK-OUT-SEL-ATTR(WRK-IND)
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

           MOVE    WRK-INP-TIMEST-EDTM TO          COMU-TIMESTAMP.
           MOVE   'DCOM0504'           TO          COMU-TRANCODE.
           MOVE    LENGTH              OF          COMU-AREA
                                       TO          COMU-LL.
           MOVE   'DCOM0502'           TO          WRK-TELA.
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
           MOVE   'DCOM0504'           TO          COMU-TRANCODE
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
           MOVE   'DCOM0504'           TO          COMU-TRANCODE
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

           IF  WRK-INP-PAG-EDTM-N      EQUAL 1
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(28)    TO WRK-OUT-MSGEM-EDTM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0550-99-FIM
           END-IF.

           COMPUTE 472X-E-RESTART      = (WRK-INP-PAG-EDTM-N - 2) * 10

           PERFORM 0210-00-CH-DCOM472X.
           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0550-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           COMPUTE WRK-OUT-PAG-EDTM-N = WRK-INP-PAG-EDTM-N - 1.

      *----------------------------------------------------------------*
       0550-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION TRATAMENTO AVANCO DE TELA NA PAGINACAO              *
      ******************************************************************
       0560-00-TELA-POSTERIOR          SECTION.
      *----------------------------------------------------------------*

           IF  WRK-INP-FIMP-EDTM       EQUAL 'S'
               PERFORM 0511-00-DEVOLVER-OCORR
               MOVE SG-MENSAGEM(29)    TO WRK-OUT-MSGEM-EDTM
               MOVE WRK-49345          TO WRK-OUT-SEL-ATTR(1)
               GO                      TO 0560-99-FIM
           END-IF.

           COMPUTE 472X-E-RESTART       = (WRK-INP-PAG-EDTM-N * 10)

           PERFORM 0210-00-CH-DCOM472X.
           IF  WRK-INCONSIS            NOT EQUAL SPACES
               GO TO 0560-99-FIM
           END-IF.

           PERFORM 3000-00-MONTA-TELA.

           COMPUTE WRK-OUT-PAG-EDTM-N   = WRK-INP-PAG-EDTM-N + 1.

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

             IF    WRK-INP-SEL-EDTM(WRK-IND)
                                       NOT EQUAL   SPACES AND LOW-VALUES

                   MOVE  WRK-49353     TO  WRK-OUT-SEL-ATTR(WRK-IND)
               IF  WRK-INP-SEL-EDTM(WRK-IND)       EQUAL  'X'
                   ADD   1             TO          WRK-COUNT
                   MOVE  WRK-IND       TO          WRK-INDT
               ELSE
                   MOVE '*'            TO          WRK-INCONSIS
                   MOVE  WRK-49369     TO  WRK-OUT-SEL-ATTR(WRK-IND)
               END-IF
             END-IF

           END-PERFORM.

           IF  WRK-INCONSIS            EQUAL '*'
               MOVE SG-MENSAGEM(086)   TO WRK-OUT-MSGEM-EDTM
           ELSE
               IF  WRK-COUNT           EQUAL ZEROS
                   MOVE '*'            TO WRK-INCONSIS
                   MOVE SG-MENSAGEM(214)
                                       TO WRK-OUT-MSGEM-EDTM
               ELSE
                   IF  WRK-COUNT       GREATER 1
                       MOVE '*'        TO WRK-INCONSIS
                       MOVE SG-MENSAGEM(215)
                                       TO WRK-OUT-MSGEM-EDTM
                   ELSE
                       PERFORM 0571-50-ACESSAR-DCOM5335
                   END-IF
               END-IF
           END-IF.

      *----------------------------------------------------------------*
       1010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      *    VERIFICAR PERMICAO DE ESTORNO DE OPERACAO                   *
      ******************************************************************
       0571-50-ACESSAR-DCOM5335        SECTION.
      *----------------------------------------------------------------*

           INITIALIZE 5335-ENTRADA
                      5335-SAIDA.

           MOVE WRK-INP-OPER-EDTM     (WRK-INDT)
                                       TO 5335E-OPERACAO.
           MOVE 006                    TO 5335E-CELMTO-DESC-COML.
           MOVE 009                    TO 5335E-CEVNTO-DESC-COML.

           EVALUATE WRK-INP-OPCAO-EDTM
               WHEN 1
                    MOVE 'I'           TO 5335E-ACAO
               WHEN 2
                    MOVE 'E'           TO 5335E-ACAO
               WHEN 3
                    GO TO 0571-50-99-FIM
           END-EVALUATE.

           MOVE 'DCOM5335'             TO WRK-MODULO.

           CALL WRK-MODULO             USING 5335-ENTRADA
                                             5335-SAIDA
                                             ERRO-AREA
                                             WRK-SQLCA.

           EVALUATE  5335S-COD-RETORNO
               WHEN '0000'
                     CONTINUE

               WHEN '0099'
                     MOVE 'DB2'        TO ERR-TIPO-ACESSO
                     PERFORM 9995-00-PROCESSAR-ROTINA-ERRO

               WHEN  OTHER
                     MOVE 5335S-MSG-RETORNO
                                       TO WRK-OUT-MSGEM-EDTM
                     MOVE '*'          TO WRK-INCONSIS
                     MOVE WRK-49369    TO WRK-OUT-SEL-ATTR(WRK-INDT)
           END-EVALUATE.

      *----------------------------------------------------------------*
       0571-50-99-FIM.                 EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      *    SECTION TRATAMENTO CONTROLE TRANSFERENCIA OUTROS PROGRAMAS  *
      ******************************************************************
       0572-00-TRANSFERIR-CONTROLE     SECTION.
      *----------------------------------------------------------------*

           INITIALIZE                  COMU-AREA.

           MOVE    WRK-INP-OPCAO-EDTM  TO          COMU-TPOPCAO.
           MOVE    WRK-INP-TIMEST-EDTM TO          COMU-TIMESTAMP.
           MOVE   'DCOM0504'           TO          COMU-TRANCODE.

           MOVE    LENGTH              OF          COMU-AREA
                                       TO          COMU-LL.
           MOVE    WRK-INP-OPER-EDTM(WRK-INDT) TO  COMU-OPERACAO.
           MOVE    WRK-INP-PAG-EDTM-N          TO  COMU-PAGINA1
           MOVE    237                         TO  COMU-BANCO

           IF      WRK-INP-PAG-EDTM-N          EQUAL 1  AND
                   WRK-INP-OPER-EDTM(2)        NOT NUMERIC
                   MOVE 1                      TO  COMU-OCCUR
           END-IF

           IF      WRK-INP-TPLI-EDTM   EQUAL       1
                   MOVE WRK-INP-LINHA1-EDTM    TO  WRK-LINHA-CPF-CNPJ
                   MOVE WRK-NUMERO(1:3)        TO  COMU-CNPJCPF(1:3)
                   MOVE WRK-NUMERO(5:3)        TO  COMU-CNPJCPF(4:3)
                   MOVE WRK-NUMERO(9:3)        TO  COMU-CNPJCPF(7:3)
                   MOVE WRK-FILIAL             TO  COMU-FILIAL
                   MOVE WRK-CONTROLE           TO  COMU-CONTROLE
           ELSE
                   MOVE WRK-INP-LINHA2-EDTM    TO  WRK-LINHA-AGENCIA
                   MOVE WRK-AGENCIA-NUM        TO  COMU-AGENCIA
                   MOVE WRK-CONTA              TO  COMU-CONTA
           END-IF

           EVALUATE  WRK-INP-OPCAO-EDTM
               WHEN  1
                   MOVE 'DCOM0505'     TO          WRK-TELA
               WHEN  2
                   MOVE 'DCOM0506'     TO          WRK-TELA
               WHEN  3
                   MOVE 'DCOM0507'     TO          WRK-TELA
               WHEN  OTHER
                   MOVE 'OPCAO INVALIDA '
                                       TO          WRK-OUT-MSGEM-EDTM
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

           MOVE LENGTH OF WRK-OUTPUT-EDTM
                                       TO  WRK-660-LL-MENSAGEM-EDTM
                                           WRK-OUT-LL-EDTM

           MOVE LENGTH OF WRK-660-DCOMEDTM
                                       TO  WRK-660-LL-AREA-EDTM

           CALL   'BRAD0660'           USING       WRK-OUTPUT-EDTM
                                                   WRK-660-DCOMEDTM.

           IF      RETURN-CODE         NOT EQUAL   ZEROS
                   MOVE 'APL'          TO          ERR-TIPO-ACESSO
                   MOVE  RETURN-CODE   TO          WRK-RETURN-CODE
                   MOVE  20            TO          WRK-LOCAL-ERRO
                   MOVE  WRK-ERRO-BRAD0660 TO      ERR-TEXTO
                   PERFORM             9995-00-PROCESSAR-ROTINA-ERRO
           END-IF.

           MOVE    WRK-ISRT            TO          WRK-FUNCAO.
           MOVE    WRK-OUTPUT-EDTM     TO          WRK-MENSAGEM.

      *----------------------------------------------------------------*
       9500-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *================================================================*
      *    SECTION CHAMADA E TRATAMENTO ROTINA BRAD7100 (ABENDS)       *
      ******************************************************************
       9995-00-PROCESSAR-ROTINA-ERRO   SECTION.
      *----------------------------------------------------------------*

           IF      ERR-MODULO          NOT EQUAL   SPACES AND LOW-VALUES
                   MOVE 'DCOM0504'     TO          ERR-PGM
           ELSE
                   MOVE 'SENHAS02'     TO          ERR-PGM
                   MOVE 'DCOM0504'     TO          ERR-MODULO
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
