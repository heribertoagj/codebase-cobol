      *****************************************************************
      * NOME DA INC - I#GFCTRY                                        *
      * DESCRICAO   - ARQUIVO CLIEADES COM INFORMACOES ADICIONAIS     *
      *                 DA AGENCIA - MUNICIPIO E UF                   *
      *                 E DA CONTA - SEGMENTO GFCT                    *
      * DATA        - MAIO/2006                                       *
ST2506* RESPONSAVEL - VINICIUS - PROCWORK - 52 POS.                   *
      *===============================================================*
ST2506*               U L T I M A   A L T E R A C A O                 *
      *---------------------------------------------------------------*
ST2506*    ANALISTA....:  STEFANINI                                   *
ST2506*    DATA........:  06 / 2025                                   *
ST2506*    OBJETIVO....:  CNPJ ALFANUMERICO                           *
      *===============================================================*
      *
      *
       01  GFCTRY-ENTRADA.
           03 GFCTRY-AGENCIA                PIC  9(005)    COMP-3.
           03 GFCTRY-RAZAO                  PIC  9(005)    COMP-3.
           03 GFCTRY-CONTA                  PIC  9(007)    COMP-3.
           03 GFCTRY-CGC-CPF                PIC  9(009)    COMP-3.
           03 GFCTRY-FILIAL                 PIC  9(005)    COMP-3.
           03 GFCTRY-CONTR                  PIC  9(002).
           03 GFCTRY-POSTO-SERV             PIC  9(005)    COMP-3.
           03 GFCTRY-DT-ABERT               PIC  9(009)    COMP-3.
           03 GFCTRY-SEGTO-GFCT             PIC  9(005)    COMP-3.
           03 GFCTRY-MUNICIPIO              PIC  9(007)    COMP-3.
           03 GFCTRY-SIGLA-UF               PIC  X(002).
ST2506     03 GFCTRY-CGC-CPF-ST             PIC  X(009).
ST2506     03 GFCTRY-FILIAL-ST              PIC  X(004).
ST2510     03 GFCTRY-CONTR-ST               PIC  9(002).
ST2506*    03 FILLER                        PIC  X(013).
