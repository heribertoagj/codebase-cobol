      *----------------------------------------------------------------*
      * NOME BOOK   - GFCTWP16                                         *
      * DESCRICAO   - DADOS DE CONTAS COM CESTAS EXCLUSIVE             *
      *               QUE FORAM ENCARTEIRADAS E QUE TEM DIREITO        *
      *               A TEST DRIVE (ISENCAO OU DESC.)                  *
      * TAMANHO     - 110                                              *
      * DATA        - JUN/2016                                         *
      * RESPONSAVEL - REGINA - SONDA IT                                *
      *                                                                *
      *----------------------------------------------------------------*
      * P16-ENCARTE-REG - DADOS DO CLIENTE E DO ENCARTEIRAMENTO        *
      * P16-AGENCIA     - CODIGO DA AGENCIA                            *
      * P16-CONTA       - NRO DA CONTA                                 *
      * P16-CCGC-CPF    - CPF DO CLIENTE                               *
      * P16-CFLIAL-CGC  - CPF DO CLIENTE                               *
      * P16-CCTRL-CPF-CGC - CPF DO CLIENTE                             *
      * P16-DT-ABERT    - DATA DA ABERTURA DA CONTA                    *
      * P16-TP-PESSOA   - TIPO DE PESSOA (F/J)                         *
      * P16-DT-ENCART   - DATA DE ENCARTEIRAMENTO DA CONTA             *
      *----------------------------------------------------------------*
      * P16-ADESAO-VIGENTE     - DADOS DA ADESAO VIGENTE               *
      * P16-CPCOTE-TARIF-VIG   - CESTA DA ADESAO VIGENTE               *
      * P16-DINIC-ADSAO-INDVD  - DATA INICIO DA VIGENCIA DA ADESAO VIG.*
      * P16-DFIM-ADSAO-INDVD   - DATA FIM  DA VIGENCIA DA ADESAO VIG.  *
      * P16-CPCOTE-TARIF-ANT   - CESTA DA ADESAO ANTERIOR              *
      *----------------------------------------------------------------*
      * P16-DT-ATUALIZ         - DATA DE PROCESSAMENTO (GERACAO DO ARQ)*
      * P16-TP-DESCONTO        - TIPO DE ISENCAO OU DESCONTO TEST-DRIVE*
      *                          1 - ISENCAO - REGRA 1                 *
      *                          2 - ISENCAO - REGRA 2                 *
      *                          3 - DESCONTO- REGRA 3                 *
      * P19-VALOR-LIQUIDO      - VALOR LIQUIDO A COBRAR                *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
      *================================================================*
      *
       01  GFCTWP16-REG.
           05 P16-ENCARTE-REG.
              10 P16-AGENCIA              PIC  9(05).
              10 P16-CONTA                PIC  9(07).
ST2507*       10 P16-CCGC-CPF             PIC  9(09).
ST2507*       10 P16-CFLIAL-CGC           PIC  9(04).
ST2507        10 P16-CCGC-CPF             PIC  X(09).
ST2507        10 P16-CFLIAL-CGC           PIC  X(04).
              10 P16-CCTRL-CPF-CGC        PIC  9(02).
              10 P16-DT-ABERT             PIC  X(10).
              10 P16-TP-PESSOA            PIC  X(01).
              10 P16-SEGM                 PIC  9(03).
              10 P16-DT-ENCART            PIC  X(10).
           05 P16-ADESAO-VIGENTE.
              10 P16-CPCOTE-TARIF-VIG     PIC  9(05).
              10 P16-DINIC-ADSAO-INDVD    PIC  X(10).
              10 P16-DFIM-ADSAO-INDVD     PIC  X(10).
              10 P16-CPCOTE-TARIF-ANT     PIC  9(05).
           05 P16-TEST-DRIVE.
              10 P16-TP-DESCONTO          PIC  9(01).
              10 P16-DT-ATUALIZ           PIC  9(08).
              10 P16-VALOR-LIQUIDO        PIC  9(09)V99.
           05 FILLER                      PIC  X(09).
      *
      *----------------------------------------------------------------*
