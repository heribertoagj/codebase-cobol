      *----------------------------------------------------------------*
      * NOME BOOK   - GFCTWP17                                         *
      * DESCRICAO   - DADOS DE CONTAS COM CESTAS EXCLUSIVE             *
      *               QUE FORAM ENCARTEIRADAS E QUE TEM DIREITO        *
      *               A TEST DRIVE (ISENCAO OU DESC.)                  *
      * TAMANHO     - 70                                               *
      * DATA        - JUN/2016                                         *
      * RESPONSAVEL - REGINA - SONDA IT                                *
      *                                                                *
      *----------------------------------------------------------------*
      * P17-ENCARTE-REG - DADOS DO CLIENTE E DO ENCARTEIRAMENTO        *
      * P17-AGENCIA     - CODIGO DA AGENCIA                            *
      * P17-CONTA       - NRO DA CONTA                                 *
      * P17-CCGC-CPF    - CPF DO CLIENTE                               *
      * P17-CFLIAL-CGC  - CPF DO CLIENTE                               *
      * P17-CCTRL-CPF-CGC - CPF DO CLIENTE                             *
      * P17-DT-ENCART   - DATA DE ENCARTEIRAMENTO DA CONTA             *
      * P17-DESC-ERRO   - MENSAGEM DE ERRO                             *
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWP17-REG.
           05 P17-ENCARTE-REG.
              10 P17-AGENCIA              PIC  9(05).
              10 P17-CONTA                PIC  9(07).
ST2507*       10 P17-CCGC-CPF             PIC  9(09).
ST2507*       10 P17-CFLIAL-CGC           PIC  9(04).
ST2507        10 P17-CCGC-CPF             PIC  X(09).
ST2507        10 P17-CFLIAL-CGC           PIC  X(04).
              10 P17-CCTRL-CPF-CGC        PIC  9(02).
              10 P17-DT-ENCART            PIC  X(10).
           05 P17-DESC-ERRO               PIC  X(30).
           05 FILLER                      PIC  X(03).
      *----------------------------------------------------------------*
