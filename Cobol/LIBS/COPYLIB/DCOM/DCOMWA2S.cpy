      *================================================================*
      * DESCRICAO  : CONSULTAR AGENCIA UORG (DADOS GERAIS)             *
      * COPYBOOK   : DCOMWA2S - BOOK DE RETORNO DO PROGRAMA FUNCIONAL  *
      * COORDENADOR: DCOM1ANC - CONSULTAR DADOS DA AGENCIA NO UORG     *
      * FLUXO      : DCOMIABN                                          *
      * DATA       : 05/11/2024                                        *
      * AUTOR      : FABIO AUGUSTO FINK - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWA2S-HEADER.
             10 DCOMWA2S-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWA2S'.
             10 DCOMWA2S-TAM-LAYOUT    PIC 9(005)   VALUE 01000.

             10 DCOMWA2S-BLOCO-SAIDA.
      *-->   DADOS DA AGENCIA NO UORG
      *-->   =========================================
                15 DCOMWA2S-CBCO                    PIC  9(003).
                15 DCOMWA2S-CAG-BCRIA               PIC  9(005).
                15 DCOMWA2S-CDIG-UND-ORGNZ          PIC  X(001).
                15 DCOMWA2S-CPSSOA-JURID            PIC  9(010).
                15 DCOMWA2S-NSEQ-UND-ORGNZ          PIC  9(008).
                15 DCOMWA2S-IUND-ORGNZ              PIC  X(060).
                15 DCOMWA2S-CTPO-UND-ORGNZ          PIC  9(003).
                15 DCOMWA2S-ITPO-UND-ORGNZ          PIC  X(010).
                15 DCOMWA2S-ISIT-UND-ORGNZ          PIC  X(015).
                15 DCOMWA2S-ICLASF-UND-ORGNZ        PIC  X(010).
                15 DCOMWA2S-ICLASF-AG               PIC  X(015).
                15 DCOMWA2S-CENDER                  PIC  9(010).
                15 DCOMWA2S-LOGRADOURO              PIC  X(060).
                15 DCOMWA2S-NUMERO                  PIC  X(005).
                15 DCOMWA2S-BAIRRO                  PIC  X(020).
                15 DCOMWA2S-COMPL                   PIC  X(020).
                15 DCOMWA2S-CEP                     PIC  9(005).
                15 DCOMWA2S-CEP-COMPL               PIC  9(003).
                15 DCOMWA2S-CIDADE                  PIC  X(020).
                15 DCOMWA2S-ESTADO                  PIC  X(002).
                15 DCOMWA2S-PAIS                    PIC  X(002).
                15 DCOMWA2S-CONTATO                 PIC  X(050).
                15 DCOMWA2S-EMAIL                   PIC  X(050).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWA2S-RESERVA                 PIC  X(600).
