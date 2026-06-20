# Estrutura de pastas da demanda

Capgemini (por CC): `Cobol/Projetos/Capgemini/AGROLEG-nnn/<CENTRO_CUSTO>/COBOL|JCL|...`

Stefanini / 7Comm (por componente):

```
Cobol/Projetos/<Consultoria>/<ID>/
├── README.md
├── intake.md
├── pgm/  cpy/  jcl/  net/  mfs/  db2/
└── <componente>/analise/   (fontes so para analise — nao alterar)
```

- Escopo: `pgm/`, `cpy/`, `jcl/`
- Analise: `pgm/analise/`, `cpy/analise/`, etc.
- Malha: `net/` (exports LIG*, schedule)

Ver `Cobol/Projetos/_indice/guia-organizacao-seguranca.md`.
