      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV3 - ARQUIVO RECEBIDO DO EMPF - AMORTIZACOES            *
      *  TAMANHO: 100                                                  *
      *----------------------------------------------------------------*

       01  BVVEV3-REGISTRO.
           03 BVVEV3-CNPJ          PIC  9(009).
           03 BVVEV3-FILIAL        PIC  9(004).
           03 BVVEV3-DIGITO        PIC  9(002).
           03 BVVEV3-AGENCIA       PIC  9(005).
           03 BVVEV3-CONTA         PIC  9(013).
           03 BVVEV3-DT-BAIXA      PIC  X(010).
           03 BVVEV3-CONTRATO      PIC  9(017).
           03 BVVEV3-COD-GARANT    PIC  9(003).
           03 BVVEV3-VLR-AMORT     PIC  9(015)V9(002) COMP-3.
           03 FILLER               PIC  X(028).
