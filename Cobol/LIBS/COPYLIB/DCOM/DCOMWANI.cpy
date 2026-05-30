      *================================================================*
      * DESCRICAO  : GERAR CONTRATO ARQUITETURA - BOTAO CONTRATAR      *
      * COPYBOOK   : DCOMWANI - BOOK INTERMEDIARIO ACESSO DCOM3ANI     *
      * COORDENADOR: DCOM1ANI - GERAR CONTRATO ARQUITETURA             *
      * FLUXO      : DCOMIABM                                          *
      * DATA       : 28/10/2024                                        *
      * AUTOR      : FABIO AUGUSTO FINK - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWANI-HEADER.
             10 DCOMWANI-COD-LAYOUT    PIC X(008)    VALUE 'DCOMWANI'.
             10 DCOMWANI-TAM-LAYOUT    PIC 9(005)    VALUE 01113.
      *
          05 DCOMWANI-REGISTRO.
             10 DCOMWANI-BLOCO-SESSAO.
                15 DCOMWANI-CCANAL                   PIC  9(003).
                15 DCOMWANI-CAUTEN-SEGRC.
                   20 DCOMWANI-CAUTEN-SEGRC-N        PIC  9(009).
                15 DCOMWANI-DEPENDENCIA-OPER         PIC  9(005).
                15 DCOMWANI-EMPR-USUAR-TRAB          PIC  9(010).
                15 DCOMWANI-DEPEND-USUAR-TRAB        PIC  9(008).
                15 DCOMWANI-CODIGO-DEPENDENCIA       PIC  9(005).
                15 DCOMWANI-CTERM                    PIC  X(008).
                15 DCOMWANI-RESERVA                  PIC  X(052).
      *
      *-->   AREA DE ENTRADA
      *-->   ***************
      *
             10 DCOMWANI-BLOCO-ENTRADA.
      *-->   DADOS DA PROPOSTA/SIMULACAO/CLIENTE
      *-->   =========================================
                15 DCOMWANI-E-CPPSTA-PRODT           PIC  9(019).
                15 DCOMWANI-E-NSMULA-OPER-DESC       PIC  9(009).
                15 DCOMWANI-E-CAG-BCRIA              PIC  9(005).
                15 DCOMWANI-E-CCTA-BCRIA-CLI         PIC  9(013).
                15 DCOMWANI-E-CCNPJ-CPF              PIC  X(009).
                15 DCOMWANI-E-CFLIAL-CNPJ            PIC  9(004).
                15 DCOMWANI-E-CCTRL-CNPJ-CPF         PIC  9(002).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
                15 DCOMWANI-E-FILLER                 PIC  X(440).
      *
      *-->   AREA DE SAIDA
      *-->   ***************
      *
             10 DCOMWANI-BLOCO-SAIDA.
      *
      *-->   DADOS DO CONTRATO ARQUITETURA
      *-->   =========================================
                15 DCOMWANI-S-CPSSOA-JURID-CONTR    PIC  9(010).
                15 DCOMWANI-S-CTPO-CONTR-NEGOC      PIC  9(003).
                15 DCOMWANI-S-NSEQ-CONTR-NEGOC      PIC  9(010).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWANI-S-RESERVA               PIC  X(476).
