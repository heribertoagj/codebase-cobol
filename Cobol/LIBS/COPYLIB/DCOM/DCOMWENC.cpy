      *================================================================*
      * NOME BOOK : DCOMWENC - AREA DE COMUNICACAO DCOM3ENC            *
      * DESCRICAO : CONSULTAR O NOME E O ENDERE�O DE UMA UNIDADE       *
      *             ORGANIZACIONAL (AGENCIA)                           *
      * DATA      : 16/10/2025                                         *
      * GRUPO     : CAPGEMINI                                          *
      * COMPONENTE: ESTRUTUTAL - BASEADO DOS BOOKS UORGWRFE E UORGWRFR *
      * TAMANHO   : 30000 BYTES                                        *
      *================================================================*
      *
           05 DCOMWENC-HEADER.
              10 DCOMWENC-COD-LAYOUT   PIC  X(008)     VALUE 'DCOMWENC'.
              10 DCOMWENC-TAM-LAYOUT   PIC  9(005)     VALUE 01000.

           05 DCOMWENC-REGISTRO.
      *
      *-->    DADOS DE ENTRADA
              10 DCOMWENC-BLOCO-ENTRADA.
                 15 DCOMWENC-E-CBCO             PIC  9(003).
                 15 DCOMWENC-E-CAG-BCRIA        PIC  9(005).
                 15 DCOMWENC-E-FILLER           PIC  X(092).

      *-->    DADOS DE SAIDA
              10 DCOMWENC-BLOCO-SAIDA.
                 15 DCOMWENC-S-CBCO             PIC  9(003).
                 15 DCOMWENC-S-CAG-BCRIA        PIC  9(005).
                 15 DCOMWENC-S-CDIG-UND-ORGNZ   PIC  X(001).
                 15 DCOMWENC-S-CPSSOA-JURID     PIC  9(010).
                 15 DCOMWENC-S-NSEQ-UND-ORGNZ   PIC  9(008).
                 15 DCOMWENC-S-IUND-ORGNZ       PIC  X(060).
                 15 DCOMWENC-S-CTPO-UND-ORGNZ   PIC  9(003).
                 15 DCOMWENC-S-ITPO-UND-ORGNZ   PIC  X(010).
                 15 DCOMWENC-S-ISIT-UND-ORGNZ   PIC  X(015).
                 15 DCOMWENC-S-ICLASF-UND-ORGNZ PIC  X(010).
                 15 DCOMWENC-S-ICLASF-AG        PIC  X(015).
                 15 DCOMWENC-S-CENDER           PIC  9(010).
                 15 DCOMWENC-S-LOGRADOURO       PIC  X(060).
                 15 DCOMWENC-S-NUMERO           PIC  X(005).
                 15 DCOMWENC-S-BAIRRO           PIC  X(020).
                 15 DCOMWENC-S-COMPL            PIC  X(020).
                 15 DCOMWENC-S-CEP              PIC  9(005).
                 15 DCOMWENC-S-CEP-COMPL        PIC  9(003).
                 15 DCOMWENC-S-CIDADE           PIC  X(020).
                 15 DCOMWENC-S-ESTADO           PIC  X(002).
                 15 DCOMWENC-S-PAIS             PIC  X(002).
                 15 DCOMWENC-S-CONTATO          PIC  X(050).
                 15 DCOMWENC-S-EMAIL            PIC  X(050).
                 15 FILLER                      PIC  X(500).
