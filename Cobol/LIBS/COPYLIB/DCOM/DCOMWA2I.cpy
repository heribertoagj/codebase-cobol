      *================================================================*
      * DESCRICAO  : CONSULTAR AGENCIA UORG (DADOS GERAIS)             *
      * COPYBOOK   : DCOMWA2I - BOOK INTERMEDIARIO ACESSO DCOM3ANC     *
      * COORDENADOR: DCOM1ANC - CONSULTAR DADOS DA AGENCIA NO UORG     *
      * FLUXO      : DCOMIABN                                          *
      * DATA       : 04/11/2024                                        *
      * AUTOR      : FABIO AUGUSTO FINK - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWA2I-HEADER.
             10 DCOMWA2I-COD-LAYOUT    PIC X(008)    VALUE 'DCOMWA2I'.
             10 DCOMWA2I-TAM-LAYOUT    PIC 9(005)    VALUE 01313.
      *
          05 DCOMWA2I-REGISTRO.
             10 DCOMWA2I-BLOCO-SESSAO.
                15 DCOMWA2I-CCANAL                   PIC  9(003).
                15 DCOMWA2I-CAUTEN-SEGRC.
                   20 DCOMWA2I-CAUTEN-SEGRC-N        PIC  9(009).
                15 DCOMWA2I-DEPENDENCIA-OPER         PIC  9(005).
                15 DCOMWA2I-EMPR-USUAR-TRAB          PIC  9(010).
                15 DCOMWA2I-DEPEND-USUAR-TRAB        PIC  9(008).
                15 DCOMWA2I-CODIGO-DEPENDENCIA       PIC  9(005).
                15 DCOMWA2I-CTERM                    PIC  X(008).
                15 DCOMWA2I-RESERVA                  PIC  X(052).
      *
      *-->   AREA DE ENTRADA
      *-->   ***************
      *
             10 DCOMWA2I-BLOCO-ENTRADA.
      *-->   DADOS DO BANCO/AGENCIA PARA CONSULTA
      *-->   =========================================
                15 DCOMWA2I-E-CBCO                   PIC  9(003).
                15 DCOMWA2I-E-CAG-BCRIA              PIC  9(005).
      *
      *-->   AREA RESERVADA PARA EXPANSAO
      *-->   =========================================
                15 DCOMWA2I-E-FILLER                 PIC  X(192).
      *
      *-->   AREA DE SAIDA
      *-->   ***************
      *
             10 DCOMWA2I-BLOCO-SAIDA.
      *
      *-->   DADOS DA AGENCIA NO UORG
      *-->   =========================================
                15 DCOMWA2I-S-CBCO                   PIC  9(003).
                15 DCOMWA2I-S-CAG-BCRIA              PIC  9(005).
                15 DCOMWA2I-S-CDIG-UND-ORGNZ         PIC  X(001).
                15 DCOMWA2I-S-CPSSOA-JURID           PIC  9(010).
                15 DCOMWA2I-S-NSEQ-UND-ORGNZ         PIC  9(008).
                15 DCOMWA2I-S-IUND-ORGNZ             PIC  X(060).
                15 DCOMWA2I-S-CTPO-UND-ORGNZ         PIC  9(003).
                15 DCOMWA2I-S-ITPO-UND-ORGNZ         PIC  X(010).
                15 DCOMWA2I-S-ISIT-UND-ORGNZ         PIC  X(015).
                15 DCOMWA2I-S-ICLASF-UND-ORGNZ       PIC  X(010).
                15 DCOMWA2I-S-ICLASF-AG              PIC  X(015).
                15 DCOMWA2I-S-CENDER                 PIC  9(010).
                15 DCOMWA2I-S-LOGRADOURO             PIC  X(060).
                15 DCOMWA2I-S-NUMERO                 PIC  X(005).
                15 DCOMWA2I-S-BAIRRO                 PIC  X(020).
                15 DCOMWA2I-S-COMPL                  PIC  X(020).
                15 DCOMWA2I-S-CEP                    PIC  9(005).
                15 DCOMWA2I-S-CEP-COMPL              PIC  9(003).
                15 DCOMWA2I-S-CIDADE                 PIC  X(020).
                15 DCOMWA2I-S-ESTADO                 PIC  X(002).
                15 DCOMWA2I-S-PAIS                   PIC  X(002).
                15 DCOMWA2I-S-CONTATO                PIC  X(050).
                15 DCOMWA2I-S-EMAIL                  PIC  X(050).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWA2I-S-RESERVA                PIC  X(613).
