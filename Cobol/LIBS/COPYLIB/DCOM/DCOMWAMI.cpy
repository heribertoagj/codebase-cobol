      *================================================================*
      * DESCRICAO  : INCLUIR OPERACAO DE DESCONTO - BOTAO CONTRATAR    *
      * COPYBOOK   : DCOMWAMI - BOOK INTERMEDIARIO ACESSO DCOM3AMI     *
      * COORDENADOR: DCOM1AMI - CONTRATACAO DA OPERACAO                *
      * FLUXO      : DCOMIABL                                          *
      * DATA       : 11/10/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWAMI-HEADER.
             10 DCOMWAMI-COD-LAYOUT    PIC X(008)    VALUE 'DCOMWAMI'.
             10 DCOMWAMI-TAM-LAYOUT    PIC 9(005)    VALUE 05000.
      *
          05 DCOMWAMI-REGISTRO.
             10 DCOMWAMI-BLOCO-SESSAO.
                15 DCOMWAMI-CCANAL                   PIC  9(003).
                15 DCOMWAMI-CAUTEN-SEGRC.
                   20 DCOMWAMI-CAUTEN-SEGRC-N        PIC  9(009).
                15 DCOMWAMI-DEPENDENCIA-OPER         PIC  9(005).
                15 DCOMWAMI-EMPR-USUAR-TRAB          PIC  9(010).
                15 DCOMWAMI-DEPEND-USUAR-TRAB        PIC  9(008).
                15 DCOMWAMI-CODIGO-DEPENDENCIA       PIC  9(005).
                15 DCOMWAMI-CTERM                    PIC  X(008).
                15 DCOMWAMI-RESERVA                  PIC  X(052).
      *
      *-->   AREA DE ENTRADA
      *-->   ***************
      *
             10 DCOMWAMI-BLOCO-ENTRADA.
      *-->   DADOS DA PROPOSTA/SIMULACAO/OPERACAO
      *-->   =========================================
                15 DCOMWAMI-E-CPPSTA-PRODT           PIC  9(019).
                15 DCOMWAMI-E-NSMULA-OPER-DESC       PIC  9(009).
                15 DCOMWAMI-E-DANO-OPER-DESC         PIC  9(004).
                15 DCOMWAMI-E-NSEQ-OPER-DESC         PIC  9(009).
                15 DCOMWAMI-E-DINIC-OPER-DESC        PIC  X(010).
                15 DCOMWAMI-E-CSIT-DESC-COML         PIC  9(003).
                15 DCOMWAMI-E-CPRODT                 PIC  9(003).
                15 DCOMWAMI-E-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWAMI-E-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWAMI-E-CTPO-DESC-COML         PIC  9(003).
                15 DCOMWAMI-E-CBCO                   PIC  9(003).
                15 DCOMWAMI-E-CAG-BCRIA              PIC  9(005).
                15 DCOMWAMI-E-CCTA-BCRIA-CLI         PIC  9(013).
                15 DCOMWAMI-E-CBCO-OPER-OPER         PIC  9(003).
                15 DCOMWAMI-E-CAG-OPER-OPER          PIC  9(005).
                15 DCOMWAMI-E-CCONTR-CONVE-DESC      PIC  9(009).
                15 DCOMWAMI-E-CVRSAO-CONTR-CONVE     PIC  9(003).
                15 DCOMWAMI-E-CELMTO-DESC-COML       PIC  9(003).
                15 DCOMWAMI-E-CINDCD-DIGTC-AG        PIC  X(001).
                15 DCOMWAMI-E-VOPER-DESC-COML        PIC  9(015)V9(002).
                15 DCOMWAMI-E-TOPER-DESC-APURA       PIC  9(003).
                15 DCOMWAMI-E-VOPER-DESC-APURA       PIC  9(015)V9(002).
                15 DCOMWAMI-E-QTITLO-OPER-APURA      PIC  9(005).
                15 DCOMWAMI-E-DVCTO-FNAL-APURA       PIC  X(010).
                15 DCOMWAMI-E-TMED-PONDE-OPER        PIC  9(003).
                15 DCOMWAMI-E-CMEIO-ENTRD-DESC       PIC  9(003).
                15 DCOMWAMI-E-CTPO-TX-JURO-DESC      PIC  9(003).
                15 DCOMWAMI-E-PALIQT-IOF-DESC        PIC  9(003)V9(005).
                15 DCOMWAMI-E-PTX-JURO-MES           PIC  9(003)V9(005).
                15 DCOMWAMI-E-PTX-JURO-ANO           PIC  9(003)V9(005).
                15 DCOMWAMI-E-VJURO-OPER-DESC        PIC  9(015)V9(002).
                15 DCOMWAMI-E-VTARIF-REG-TITLO       PIC  9(015)V9(002).
                15 DCOMWAMI-E-VTAC-OPER-DESC         PIC  9(015)V9(002).
                15 DCOMWAMI-E-VIOF-OPER-DESC         PIC  9(015)V9(002).
                15 DCOMWAMI-E-VLIQ-OPER-DESC         PIC  9(015)V9(002).
                15 DCOMWAMI-E-DINCL-REG              PIC  X(010).
                15 DCOMWAMI-E-PTX-CUSTO-SPREAD       PIC  9(003)V9(003).
                15 DCOMWAMI-E-QTITLO-CHEQ-PCELA      PIC  9(009).
                15 DCOMWAMI-E-CINDCD-ALIQT-REDZD     PIC  X(001).
                15 DCOMWAMI-E-PTX-JURO-DIA           PIC  9(003)V9(005).
                15 DCOMWAMI-E-CINDCD-FREQ-TX         PIC  X(002).
                15 DCOMWAMI-E-DANO-BASE              PIC  9(004).
                15 DCOMWAMI-E-CDEPDC                 PIC  9(005).
                15 DCOMWAMI-E-CSEQ-STUDO             PIC  9(009).
                15 DCOMWAMI-E-CCART-LIM-CREDT        PIC  X(005).
                15 DCOMWAMI-E-NSEQ-CONTR-LIM         PIC  9(017).
                15 DCOMWAMI-E-CPSSOA-JURID-CONTR     PIC  9(010).
                15 DCOMWAMI-E-CTPO-CONTR-NEGOC       PIC  9(003).
                15 DCOMWAMI-E-NSEQ-CONTR-NEGOC       PIC  9(010).
                15 DCOMWAMI-E-CTPO-CANAL             PIC  9(003).
                15 DCOMWAMI-E-CAUTRZ-TX-TESOU        PIC  9(009).
                15 DCOMWAMI-E-SELEC-CATAO-CREDT      PIC  X(001).
                15 DCOMWAMI-E-PTX-CUSTO-MES          PIC  9(003)V9(006).
                15 DCOMWAMI-E-PTX-CUSTO-ANO          PIC  9(003)V9(006).
      *
      *-->   DADOS DA TAXA/ENQUADRAMENTO
      *-->   =========================================
      *-->   INTERVALO DE TAXA DO PRODUTO (SEGURANCA)
                15 DCOMWAMI-E-PTX-MIN-PROD-DESC      PIC  9(003)V9(003).
                15 DCOMWAMI-E-PTX-MAX-JURO-DESC      PIC  9(003)V9(003).
                15 DCOMWAMI-E-CINDCD-FREQ-TX-DESC    PIC  X(002).
      *
      *-->   INTERVALO DE TAXA DO SUBPRODUTO (MELHOR ENQUAD. PROPOSTA)
                15 DCOMWAMI-E-PTX-MIN-SPROD-DESC     PIC  9(003)V9(003).
                15 DCOMWAMI-E-PTX-MAX-SPROD-DESC     PIC  9(003)V9(003).
                15 DCOMWAMI-E-CINDCD-FREQ-TX-SPRO    PIC  X(002).
      *
      *-->   TAXA MINIMA ESPECIAL DO CLIENTE
                15 DCOMWAMI-E-PTX-JURO-ESP-CLI       PIC  9(003)V9(003).
                15 DCOMWAMI-E-CINDCD-FREQ-TX-CLIE    PIC  X(002).
      *
      *-->   OPCAO DO CLIENTE PARA O CALCULO DE IOF
      *-->   =========================================
      *-->   -> REDUZ IOF? "S" - REDUZIDA/ "N" - NORMAL
                15 DCOMWAMI-E-FLAG-RED-IOF           PIC  X(001).
      *-->   -> ADICIONA IOF?  "S" - SIM / "N" - NAO
                15 DCOMWAMI-E-FLAG-ADIC-IOF          PIC  X(001).
      *-->   -> CALCULA CUSTO? "S" - SIM / "N" - NAO
                15 DCOMWAMI-E-FLAG-CALC-CUSTO        PIC  X(001).
      *
      *-->   DADOS COMPLEMENTARES DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-E-CVAR-PRODT-RURAL       PIC  9(005).
                15 DCOMWAMI-E-CNRO-REFT-BACEN        PIC  9(018).
                15 DCOMWAMI-E-CCESTA-SAFRA-RURAL     PIC  9(005).
                15 DCOMWAMI-E-CINDCD-ZNMTO           PIC  9(001).
                15 DCOMWAMI-E-CTPO-BNEFC-RURAL       PIC  9(002).
                15 DCOMWAMI-E-NBASE-LEGAL-RENEG      PIC  9(005).
                15 DCOMWAMI-E-CREG-AMBTL-RURAL       PIC  X(041).
                15 DCOMWAMI-E-CORIGE-ANTEC-PGMD      PIC  9(001).
                15 DCOMWAMI-E-CCONTR-LIM-DESC        PIC  9(009).
                15 DCOMWAMI-E-CVRSAO-CONTR-LIM       PIC  9(003).
                15 DCOMWAMI-E-CINDCD-FORMA-LIQDC     PIC  X(001).
                15 DCOMWAMI-E-CINDCD-UTILZ-LIM       PIC  X(001).
                15 DCOMWAMI-E-CINDCD-DEB-PCIAL       PIC  X(001).
                15 DCOMWAMI-E-CINDCD-TENTV-DEB       PIC  X(001).
      *
      *-->   DADOS DO CEDENTE/PROPONENTE DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-E-CCNPJ-CPF              PIC  9(009).
                15 DCOMWAMI-E-CFLIAL-CNPJ            PIC  9(004).
                15 DCOMWAMI-E-CCTRL-CNPJ-CPF         PIC  9(002).
                15 DCOMWAMI-E-CDOCTO-ID              PIC  X(015).
                15 DCOMWAMI-E-IPSSOA-DESC-COML       PIC  X(060).
                15 DCOMWAMI-E-CCLUB                  PIC  9(010).
                15 DCOMWAMI-E-CCEP                   PIC  9(005).
                15 DCOMWAMI-E-CCEP-COMPL             PIC  9(003).
                15 DCOMWAMI-E-ELOGDR                 PIC  X(040).
                15 DCOMWAMI-E-ELOGDR-NRO             PIC  X(007).
                15 DCOMWAMI-E-CSGL-UF                PIC  X(002).
      *
      *-->   DADOS DAS TESTEMUNHAS DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-E-TESTEMUNHAS            OCCURS 002 TIMES.
                   20 DCOMWAMI-E-CCNPJ-CPF-TSTM      PIC  9(009).
                   20 DCOMWAMI-E-CFLIAL-CNPJ-TSTM    PIC  9(004).
                   20 DCOMWAMI-E-CCTRL-CNPJCPF-TSTM  PIC  9(002).
                   20 DCOMWAMI-E-CDOCTO-ID-TSTM      PIC  X(015).
                   20 DCOMWAMI-E-CCLUB-TSTM          PIC  9(010).
                   20 DCOMWAMI-E-IPSSOA-DESC-TSTM    PIC  X(060).
      *
      *-->   DADOS DO MEIO DE LIBERACAO DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-E-CMEIO-LIBRC-DESC       PIC  9(003).
                15 DCOMWAMI-E-NOPER-MEIO-LIBRC       PIC  9(003).
                15 DCOMWAMI-E-CBCO-LIBRC             PIC  9(003).
                15 DCOMWAMI-E-CAG-BCRIA-LIBRC        PIC  9(005).
                15 DCOMWAMI-E-CCTA-BCRIA-LIBRC       PIC  9(013).
                15 DCOMWAMI-E-CCNPJ-CPF-LIBRC        PIC  9(009).
                15 DCOMWAMI-E-CFLIAL-CNPJ-LIBRC      PIC  9(004).
                15 DCOMWAMI-E-CCTRL-CNPJCPF-LBR      PIC  9(002).
                15 DCOMWAMI-E-IPSSOA-DESC-LIBRC      PIC  X(060).
                15 DCOMWAMI-E-CCLUB-LIBRC            PIC  9(010).
                15 DCOMWAMI-E-PLIBRC-DESC-COML       PIC  9(003)V9(003).
      *
      *-->   DADOS DE CUSTO EFETIVO TOTAL DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-E-VCUSTO-DESC-DIA        PIC  9(008)V9(007).
                15 DCOMWAMI-E-VCUSTO-DESC-MES        PIC  9(008)V9(007).
                15 DCOMWAMI-E-VCUSTO-DESC-ANO        PIC  9(008)V9(007).
      *
      *-->   DADOS DE PARAMETROS DO SUBPRODUTO
      *-->   =========================================
      *-->   -> PARAMETROS DO SUBPRODUTO DCOMB064 - TSPROD_MEIO_ENTRD
                15 DCOMWAMI-E-CINDCD-LIBRC-ANTCP     PIC  X(001).
                15 DCOMWAMI-E-CINDCD-FORML-OPER      PIC  X(001).
      *-->   -> PARAMETROS DO SUBPRODUTO DCOMB068 - TSPROD-REGRA-OPER
                15 DCOMWAMI-E-CINDCD-PGDOR-JURO      PIC  X(001).
                15 DCOMWAMI-E-CINDCD-CALC-JURO       PIC  X(001).
                15 DCOMWAMI-E-CINDCD-INCID-IOF       PIC  X(001).
                15 DCOMWAMI-E-FILLER                 PIC  X(096).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
                15 DCOMWAMI-E-FILLER                 PIC  X(1855).
      *
      *-->   AREA DE SAIDA
      *-->   ***************
      *
             10 DCOMWAMI-BLOCO-SAIDA.
      *
      *-->   DADOS DA OPERACAO CADASTRADA
      *-->   =========================================
                15 DCOMWAMI-S-DANO-OPER-DESC        PIC  9(004).
                15 DCOMWAMI-S-NSEQ-OPER-DESC        PIC  9(009).
                15 DCOMWAMI-S-CSIT-DESC-COML        PIC  9(003).
                15 DCOMWAMI-S-ISIT-DESC-COML        PIC  X(040).
      *
      *-->   DADOS DO CEDENTE/PROPONENTE DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-S-NPSSOA-DESC-COML      PIC  9(009).
                15 DCOMWAMI-S-NRELAC-PSSOA-DESC     PIC  9(009).
      *
      *-->   DADOS DE TESTEMUNHAS DA OPERACAO
      *-->   =========================================
                15 DCOMWAMI-S-TESTEMUNHAS           OCCURS 002 TIMES.
                   20 DCOMWAMI-S-NPSSOA-DESC-TSTM   PIC  9(009).
                   20 DCOMWAMI-S-NRELAC-PSSOA-TSTM  PIC  9(009).
                   20 DCOMWAMI-S-CCNPJ-CPF-TSTM     PIC  9(009).
                   20 DCOMWAMI-S-CCTRL-CNPJCPF-TSTM PIC  9(002).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWAMI-S-RESERVA               PIC  X(1730).
