      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV4 - ARQUIVO RECEBIDO DO EMPF - AMORTIZACOES COM BANDEIRA
      *  TAMANHO: 160                                                  *
      *----------------------------------------------------------------*

       01  BVVEV4-REGISTRO.
           03 BVVEV4-CNPJ          PIC  9(009).
           03 BVVEV4-FILIAL        PIC  9(004).
           03 BVVEV4-DIGITO        PIC  9(002).
           03 BVVEV4-AGENCIA       PIC  9(005).
           03 BVVEV4-CONTA         PIC  9(013).
           03 BVVEV4-DT-BAIXA      PIC  X(010).
           03 BVVEV4-CONTRATO      PIC  9(017).
           03 BVVEV4-COD-GARANT    PIC  9(003).
           03 BVVEV4-VLR-AMORT     PIC  9(015)V9(002) COMP-3.
           03 BVVEV4-BANDEIRA      PIC  X(015).
           03 BVVEV4-COD-BAND      PIC  9(003).
           03 BVVEV4-AGEN-GAR      PIC  9(005).
           03 BVVEV4-CONT-GAR      PIC  9(013).
           03 FILLER               PIC  X(052).
